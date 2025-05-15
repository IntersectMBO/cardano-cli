{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Legacy.Governance.Run
  ( runLegacyGovernanceCmds
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Governance.GenesisKeyDelegationCertificate.Run
  ( runGovernanceGenesisKeyDelegationCertificate
  )
import Cardano.CLI.EraBased.Governance.Run
import Cardano.CLI.Legacy.Governance.Command
import Cardano.CLI.Orphan ()
import Cardano.CLI.Read
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.GovernanceCmdError

import Control.Monad
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as LB

runLegacyGovernanceCmds :: LegacyGovernanceCmds -> CIO e ()
runLegacyGovernanceCmds = \case
  GovernanceCreateMirCertificateStakeAddressesCmd anyEra mirpot vKeys rewards out ->
    runLegacyGovernanceMIRCertificatePayStakeAddrs anyEra mirpot vKeys rewards out
  GovernanceCreateMirCertificateTransferToTreasuryCmd anyEra amt out -> do
    runLegacyGovernanceCreateMirCertificateTransferToTreasuryCmd anyEra amt out
  GovernanceCreateMirCertificateTransferToReservesCmd anyEra amt out -> do
    runLegacyGovernanceCreateMirCertificateTransferToReservesCmd anyEra amt out
  GovernanceGenesisKeyDelegationCertificate (EraInEon sbe) genVk genDelegVk vrfVk out ->
    runGovernanceGenesisKeyDelegationCertificate sbe genVk genDelegVk vrfVk out
  GovernanceUpdateProposal out eNo genVKeys ppUp mCostModelFp ->
    runLegacyGovernanceUpdateProposal out eNo genVKeys ppUp mCostModelFp

runLegacyGovernanceMIRCertificatePayStakeAddrs
  :: EraInEon ShelleyToBabbageEra
  -> L.MIRPot
  -> [StakeAddress]
  -- ^ Stake addresses
  -> [Lovelace]
  -- ^ Corresponding reward amounts (same length)
  -> File () Out
  -> CIO e ()
runLegacyGovernanceMIRCertificatePayStakeAddrs (EraInEon w) =
  runGovernanceMIRCertificatePayStakeAddrs w

runLegacyGovernanceCreateMirCertificateTransferToTreasuryCmd
  :: EraInEon ShelleyToBabbageEra
  -> Lovelace
  -> File () Out
  -> CIO e ()
runLegacyGovernanceCreateMirCertificateTransferToTreasuryCmd (EraInEon w) =
  runGovernanceCreateMirCertificateTransferToTreasuryCmd w

runLegacyGovernanceCreateMirCertificateTransferToReservesCmd
  :: EraInEon ShelleyToBabbageEra
  -> Lovelace
  -> File () Out
  -> CIO e ()
runLegacyGovernanceCreateMirCertificateTransferToReservesCmd (EraInEon w) =
  runGovernanceCreateMirCertificateTransferToReservesCmd w

runLegacyGovernanceUpdateProposal
  :: File () Out
  -> EpochNo
  -> [VerificationKeyFile In]
  -- ^ Genesis verification keys
  -> ProtocolParametersUpdate
  -> Maybe FilePath
  -- ^ Cost models file path
  -> CIO e ()
runLegacyGovernanceUpdateProposal upFile eNo genVerKeyFiles upPprams mCostModelFp = do
  finalUpPprams <- case mCostModelFp of
    Nothing -> return upPprams
    Just fp -> do
      costModelsBs <- readFileCli fp

      cModels <-
        fromEitherCli (eitherDecode $ LB.fromStrict costModelsBs)

      let costModels = fromAlonzoCostModels cModels

      when (null costModels) $ throwCliError (GovernanceCmdEmptyCostModel fp)

      return $ upPprams{protocolUpdateCostModels = costModels}

  when (finalUpPprams == mempty) $ throwCliError GovernanceCmdEmptyUpdateProposalError

  genVKeys <-
    sequence
      [ fromEitherIOCli $
          readFileTextEnvelope vkeyFile
      | vkeyFile <- genVerKeyFiles
      ]
  let genKeyHashes = fmap verificationKeyHash genVKeys
      upProp = makeShelleyUpdateProposal finalUpPprams genKeyHashes eNo

  fromEitherIOCli @(FileError ()) $
    writeLazyByteStringFile upFile $
      textEnvelopeToJSON Nothing upProp
