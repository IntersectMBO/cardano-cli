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
  , runGovernanceCreateMirCertificateTransferToTreasuryCmd
  , runGovernanceCreateMirCertificateTransferToReservesCmd
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley

import qualified Cardano.CLI.EraBased.Commands.Governance as Cmd
import           Cardano.CLI.EraBased.Run.Governance.Actions
import           Cardano.CLI.EraBased.Run.Governance.Committee
import           Cardano.CLI.EraBased.Run.Governance.DRep
import           Cardano.CLI.EraBased.Run.Governance.GenesisKeyDelegationCertificate
import           Cardano.CLI.EraBased.Run.Governance.Poll
import           Cardano.CLI.EraBased.Run.Governance.Vote
import           Cardano.CLI.Types.Errors.CmdError
import           Cardano.CLI.Types.Errors.GovernanceCmdError

import           Control.Monad
import           Data.Function
import           GHC.Exts (IsList (..))

runGovernanceCmds
  :: ()
  => Cmd.GovernanceCmds era
  -> ExceptT CmdError IO ()
runGovernanceCmds = \case
  Cmd.GovernanceCreateMirCertificateStakeAddressesCmd w mirpot vKeys rewards out ->
    runGovernanceMIRCertificatePayStakeAddrs w mirpot vKeys rewards out
      & firstExceptT CmdGovernanceCmdError
  Cmd.GovernanceCreateMirCertificateTransferToTreasuryCmd w ll oFp ->
    runGovernanceCreateMirCertificateTransferToTreasuryCmd w ll oFp
      & firstExceptT CmdGovernanceCmdError
  Cmd.GovernanceCreateMirCertificateTransferToReservesCmd w ll oFp ->
    runGovernanceCreateMirCertificateTransferToReservesCmd w ll oFp
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
  :: forall era
   . ShelleyToBabbageEra era
  -> L.MIRPot
  -> [StakeAddress]
  -- ^ Stake addresses
  -> [Lovelace]
  -- ^ Corresponding reward amounts (same length)
  -> File () Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceMIRCertificatePayStakeAddrs w mirPot sAddrs rwdAmts oFp = do
  unless (length sAddrs == length rwdAmts) $
    left $
      GovernanceCmdMIRCertificateKeyRewardMistmach
        (unFile oFp)
        (length sAddrs)
        (length rwdAmts)

  let sCreds = map stakeAddressCredential sAddrs
      mirTarget =
        L.StakeAddressesMIR $
          fromList
            [ (toShelleyStakeCredential scred, L.toDeltaCoin rwdAmt)
            | (scred, rwdAmt) <- zip sCreds rwdAmts
            ]
  let mirCert =
        makeMIRCertificate $
          MirCertificateRequirements w mirPot $
            shelleyToBabbageEraConstraints w mirTarget
      sbe = convert w

  firstExceptT GovernanceCmdTextEnvWriteError
    . newExceptT
    $ shelleyBasedEraConstraints sbe
    $ writeLazyByteStringFile oFp
    $ textEnvelopeToJSON (Just mirCertDesc) mirCert
 where
  mirCertDesc :: TextEnvelopeDescr
  mirCertDesc = "Move Instantaneous Rewards Certificate"

runGovernanceCreateMirCertificateTransferToTreasuryCmd
  :: forall era
   . ()
  => ShelleyToBabbageEra era
  -> Lovelace
  -> File () Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceCreateMirCertificateTransferToTreasuryCmd w ll oFp = do
  let mirTarget = L.SendToOppositePotMIR ll

  let mirCert = makeMIRCertificate $ MirCertificateRequirements w L.ReservesMIR mirTarget
      sbe = convert w

  firstExceptT GovernanceCmdTextEnvWriteError
    . newExceptT
    $ shelleyBasedEraConstraints sbe
    $ writeLazyByteStringFile oFp
    $ textEnvelopeToJSON (Just mirCertDesc) mirCert
 where
  mirCertDesc :: TextEnvelopeDescr
  mirCertDesc = "MIR Certificate Send To Treasury"

runGovernanceCreateMirCertificateTransferToReservesCmd
  :: forall era
   . ()
  => ShelleyToBabbageEra era
  -> Lovelace
  -> File () Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceCreateMirCertificateTransferToReservesCmd w ll oFp = do
  let mirTarget = L.SendToOppositePotMIR ll

  let mirCert = makeMIRCertificate $ MirCertificateRequirements w L.TreasuryMIR mirTarget
      sbe = convert w

  firstExceptT GovernanceCmdTextEnvWriteError
    . newExceptT
    $ shelleyBasedEraConstraints sbe
    $ writeLazyByteStringFile oFp
    $ textEnvelopeToJSON (Just mirCertDesc) mirCert
 where
  mirCertDesc :: TextEnvelopeDescr
  mirCertDesc = "MIR Certificate Send To Reserves"
