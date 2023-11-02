{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Run.Governance
  ( runGovernanceCmds

  , runGovernanceMIRCertificatePayStakeAddrs
  , runGovernanceMIRCertificateTransfer
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import qualified Cardano.CLI.EraBased.Commands.Governance as Cmd
import           Cardano.CLI.EraBased.Run.Governance.Actions
import           Cardano.CLI.EraBased.Run.Governance.Committee
import           Cardano.CLI.EraBased.Run.Governance.DRep
import           Cardano.CLI.EraBased.Run.Governance.GenesisKeyDelegationCertificate
import           Cardano.CLI.EraBased.Run.Governance.Poll
import           Cardano.CLI.EraBased.Run.Governance.Vote
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.CmdError
import           Cardano.CLI.Types.Errors.GovernanceCmdError
import qualified Cardano.Ledger.Shelley.TxBody as Shelley

import           Control.Monad
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
import           Data.Function
import qualified Data.Map.Strict as Map

runGovernanceCmds :: ()
  => Cmd.GovernanceCmds era
  -> ExceptT CmdError IO ()
runGovernanceCmds = \case
  Cmd.GovernanceMIRPayStakeAddressesCertificate w mirpot vKeys rewards out ->
    runGovernanceMIRCertificatePayStakeAddrs w mirpot vKeys rewards out
      & firstExceptT CmdGovernanceCmdError

  Cmd.GovernanceMIRTransfer w ll oFp direction ->
    runGovernanceMIRCertificateTransfer w ll oFp direction
      & firstExceptT CmdGovernanceCmdError

  Cmd.GovernanceGenesisKeyDelegationCertificate sta genVk genDelegVk vrfVk out ->
    runGovernanceGenesisKeyDelegationCertificate sta genVk genDelegVk vrfVk out
      & firstExceptT CmdGovernanceCmdError

  Cmd.GovernanceCommitteeCmds cmds ->
    runGovernanceCommitteeCmds cmds
      & firstExceptT CmdGovernanceCommitteeError

  Cmd.GovernanceActionCmds cmds ->
    runGovernanceActionCmds cmds
      & firstExceptT CmdGovernanceActionError

  Cmd.GovernanceDRepCmds cmds ->
    runGovernanceDRepCmds cmds

  Cmd.GovernancePollCmds cmds ->
    runGovernancePollCmds cmds
      & firstExceptT CmdGovernanceCmdError

  Cmd.GovernanceVoteCmds cmds ->
    runGovernanceVoteCmds cmds

runGovernanceMIRCertificatePayStakeAddrs
  :: ShelleyToBabbageEra era
  -> Shelley.MIRPot
  -> [StakeAddress] -- ^ Stake addresses
  -> [Lovelace]     -- ^ Corresponding reward amounts (same length)
  -> File () Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceMIRCertificatePayStakeAddrs w mirPot sAddrs rwdAmts oFp = do
  unless (length sAddrs == length rwdAmts) $
    left $ GovernanceCmdMIRCertificateKeyRewardMistmach
              (unFile oFp) (length sAddrs) (length rwdAmts)

  let sCreds  = map stakeAddressCredential sAddrs
      mirTarget = Ledger.StakeAddressesMIR
                    $ Map.fromList [ (toShelleyStakeCredential scred, Ledger.toDeltaCoin (toShelleyLovelace rwdAmt))
                                    | (scred, rwdAmt) <- zip sCreds rwdAmts
                                    ]
  let mirCert = makeMIRCertificate
        $ MirCertificateRequirements w mirPot
        $ shelleyToBabbageEraConstraints w mirTarget

  firstExceptT GovernanceCmdTextEnvWriteError
    . newExceptT
    $ shelleyBasedEraConstraints (shelleyToBabbageEraToShelleyBasedEra w)
    $ writeLazyByteStringFile oFp
    $ textEnvelopeToJSON (Just mirCertDesc) mirCert
  where
    mirCertDesc :: TextEnvelopeDescr
    mirCertDesc = "Move Instantaneous Rewards Certificate"

runGovernanceMIRCertificateTransfer
  :: ShelleyToBabbageEra era
  -> Lovelace
  -> File () Out
  -> TransferDirection
  -> ExceptT GovernanceCmdError IO ()
runGovernanceMIRCertificateTransfer w ll oFp direction = do
  let mirTarget = Ledger.SendToOppositePotMIR (toShelleyLovelace ll)

  let mirCert =
        makeMIRCertificate $
          case direction of
            TransferToReserves -> MirCertificateRequirements w Ledger.TreasuryMIR mirTarget
            TransferToTreasury -> MirCertificateRequirements w Ledger.ReservesMIR mirTarget

  firstExceptT GovernanceCmdTextEnvWriteError
    . newExceptT
    $ shelleyBasedEraConstraints (shelleyToBabbageEraToShelleyBasedEra w)
    $ writeLazyByteStringFile oFp
    $ textEnvelopeToJSON (Just $ mirCertDesc direction) mirCert
  where
    mirCertDesc :: TransferDirection -> TextEnvelopeDescr
    mirCertDesc TransferToTreasury = "MIR Certificate Send To Treasury"
    mirCertDesc TransferToReserves = "MIR Certificate Send To Reserves"
