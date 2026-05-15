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
  , runGovernanceCreateMirCertificateTransferToReservesCmd
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Governance.Actions.Run
import Cardano.CLI.EraBased.Governance.Command qualified as Cmd
import Cardano.CLI.EraBased.Governance.Committee.Run
import Cardano.CLI.EraBased.Governance.DRep.Run
import Cardano.CLI.EraBased.Governance.Vote.Run
import Cardano.CLI.Type.Error.GovernanceCmdError

import RIO

import GHC.Exts (IsList (..))

runGovernanceCmds
  :: Cmd.GovernanceCmds era
  -> CIO e ()
runGovernanceCmds = \case
  Cmd.GovernanceCommitteeCmds cmds ->
    runGovernanceCommitteeCmds cmds
  Cmd.GovernanceActionCmds cmds ->
    runGovernanceActionCmds cmds
  Cmd.GovernanceDRepCmds cmds ->
    runGovernanceDRepCmds cmds
  Cmd.GovernanceVoteCmds cmds ->
    runGovernanceVoteCmds cmds

runGovernanceMIRCertificatePayStakeAddrs
  :: ShelleyToBabbageEra era
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
  let mirCert = mkMIRCert mirPot mirTarget
      sbe = convert w

  fromEitherIOCli @(FileError ()) $
    shelleyBasedEraConstraints sbe $
      writeLazyByteStringFile oFp $
        textEnvelopeToJSON (Just mirCertDesc) mirCert
 where
  mirCertDesc :: TextEnvelopeDescr
  mirCertDesc = "Move Instantaneous Rewards Certificate"

runGovernanceCreateMirCertificateTransferToReservesCmd
  :: ShelleyToBabbageEra era
  -> Lovelace
  -> File () Out
  -> CIO e ()
runGovernanceCreateMirCertificateTransferToReservesCmd w ll oFp = do
  let mirTarget = L.SendToOppositePotMIR ll

  let mirCert = mkMIRCert L.TreasuryMIR mirTarget
      sbe = convert w

  fromEitherIOCli @(FileError ()) $
    shelleyBasedEraConstraints sbe $
      writeLazyByteStringFile oFp $
        textEnvelopeToJSON (Just mirCertDesc) mirCert
 where
  mirCertDesc :: TextEnvelopeDescr
  mirCertDesc = "MIR Certificate Send To Reserves"

-- MIR certificates only exist up to the Babbage era. The serialization of
-- @ShelleyTxCert@ is uniform across Shelley→Babbage, so we anchor the
-- text envelope type to @BabbageEra@.
mkMIRCert
  :: L.MIRPot
  -> L.MIRTarget
  -> Exp.Certificate (ShelleyLedgerEra BabbageEra)
mkMIRCert mirPot mirTarget =
  Exp.Certificate $ L.ShelleyTxCertMir $ L.MIRCert mirPot mirTarget
