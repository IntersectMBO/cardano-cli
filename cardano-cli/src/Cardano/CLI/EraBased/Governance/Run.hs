{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Governance.Run
  ( runGovernanceCmds
  , runGovernanceMIRCertificatePayStakeAddrs
  , runGovernanceCreateMirCertificateTransferToTreasuryCmd
  , runGovernanceCreateMirCertificateTransferToReservesCmd
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Governance.Actions.Run
import Cardano.CLI.EraBased.Governance.Command qualified as Cmd
import Cardano.CLI.EraBased.Governance.Committee.Run
import Cardano.CLI.EraBased.Governance.DRep.Run
import Cardano.CLI.EraBased.Governance.GenesisKeyDelegationCertificate.Run
import Cardano.CLI.EraBased.Governance.Vote.Run
import Cardano.CLI.Type.Error.GovernanceCmdError

import RIO

import GHC.Exts (IsList (..))

runGovernanceCmds
  :: Typeable era
  => Cmd.GovernanceCmds era
  -> CIO e ()
runGovernanceCmds = \case
  Cmd.GovernanceGenesisKeyDelegationCertificate sta genVk genDelegVk vrfVk out ->
    runGovernanceGenesisKeyDelegationCertificate sta genVk genDelegVk vrfVk out
  Cmd.GovernanceCommitteeCmds cmds ->
    runGovernanceCommitteeCmds cmds
  Cmd.GovernanceActionCmds cmds ->
    runGovernanceActionCmds cmds
  Cmd.GovernanceDRepCmds cmds ->
    runGovernanceDRepCmds cmds
  Cmd.GovernanceVoteCmds cmds ->
    runGovernanceVoteCmds cmds

runGovernanceMIRCertificatePayStakeAddrs
  :: forall era e
   . Typeable era
  => ShelleyToBabbageEra era
  -> L.MIRPot
  -> [StakeAddress]
  -- ^ Stake addresses
  -> [Lovelace]
  -- ^ Corresponding reward amounts (same length)
  -> File () Out
  -> CIO e ()
runGovernanceMIRCertificatePayStakeAddrs w mirPot sAddrs rwdAmts oFp = do
  unless (length sAddrs == length rwdAmts) $
    throwCliError $
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

  fromEitherIOCli @(FileError ()) $
    shelleyBasedEraConstraints sbe $
      writeLazyByteStringFile oFp $
        textEnvelopeToJSON (Just mirCertDesc) mirCert
 where
  mirCertDesc :: TextEnvelopeDescr
  mirCertDesc = "Move Instantaneous Rewards Certificate"

runGovernanceCreateMirCertificateTransferToTreasuryCmd
  :: forall era e
   . Typeable era
  => ShelleyToBabbageEra era
  -> Lovelace
  -> File () Out
  -> CIO e ()
runGovernanceCreateMirCertificateTransferToTreasuryCmd w ll oFp = do
  let mirTarget = L.SendToOppositePotMIR ll

  let mirCert = makeMIRCertificate $ MirCertificateRequirements w L.ReservesMIR mirTarget
      sbe = convert w

  fromEitherIOCli @(FileError ()) $
    shelleyBasedEraConstraints sbe $
      writeLazyByteStringFile oFp $
        textEnvelopeToJSON (Just mirCertDesc) mirCert
 where
  mirCertDesc :: TextEnvelopeDescr
  mirCertDesc = "MIR Certificate Send To Treasury"

runGovernanceCreateMirCertificateTransferToReservesCmd
  :: forall era e
   . Typeable era
  => ShelleyToBabbageEra era
  -> Lovelace
  -> File () Out
  -> CIO e ()
runGovernanceCreateMirCertificateTransferToReservesCmd w ll oFp = do
  let mirTarget = L.SendToOppositePotMIR ll

  let mirCert = makeMIRCertificate $ MirCertificateRequirements w L.TreasuryMIR mirTarget
      sbe = convert w

  fromEitherIOCli @(FileError ()) $
    shelleyBasedEraConstraints sbe $
      writeLazyByteStringFile oFp $
        textEnvelopeToJSON (Just mirCertDesc) mirCert
 where
  mirCertDesc :: TextEnvelopeDescr
  mirCertDesc = "MIR Certificate Send To Reserves"
