{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Legacy.Governance.Run
  ( runLegacyGovernanceCmds
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley

import Cardano.CLI.EraBased.Governance.GenesisKeyDelegationCertificate.Run
  ( runGovernanceGenesisKeyDelegationCertificate
  )
import Cardano.CLI.EraBased.Governance.Poll.Command qualified as Cmd
import Cardano.CLI.EraBased.Governance.Poll.Run
import Cardano.CLI.EraBased.Governance.Run
import Cardano.CLI.Legacy.Governance.Command
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.GovernanceCmdError

import Control.Monad
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as LB
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text

runLegacyGovernanceCmds :: LegacyGovernanceCmds -> ExceptT GovernanceCmdError IO ()
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
  GovernanceCreatePoll prompt choices nonce out ->
    runLegacyGovernanceCreatePoll prompt choices nonce out
  GovernanceAnswerPoll poll ix mOutFile ->
    runLegacyGovernanceAnswerPoll poll ix mOutFile
  GovernanceVerifyPoll poll metadata mOutFile ->
    runLegacyGovernanceVerifyPoll poll metadata mOutFile

runLegacyGovernanceCreatePoll
  :: ()
  => Text
  -> [Text]
  -> Maybe Word
  -> File GovernancePoll Out
  -> ExceptT GovernanceCmdError IO ()
runLegacyGovernanceCreatePoll prompt choices nonce outFile =
  runGovernanceCreatePollCmd
    Cmd.GovernanceCreatePollCmdArgs
      { eon = BabbageEraOnwardsBabbage
      , prompt
      , choices
      , nonce
      , outFile
      }

runLegacyGovernanceAnswerPoll
  :: ()
  => File GovernancePoll In
  -> Maybe Word
  -> Maybe (File () Out)
  -> ExceptT GovernanceCmdError IO ()
runLegacyGovernanceAnswerPoll pollFile answerIndex mOutFile =
  runGovernanceAnswerPollCmd
    Cmd.GovernanceAnswerPollCmdArgs
      { eon = BabbageEraOnwardsBabbage
      , pollFile
      , answerIndex
      , mOutFile
      }

runLegacyGovernanceVerifyPoll
  :: ()
  => File GovernancePoll In
  -> File (Tx ()) In
  -> Maybe (File () Out)
  -> ExceptT GovernanceCmdError IO ()
runLegacyGovernanceVerifyPoll pollFile txFile mOutFile =
  runGovernanceVerifyPollCmd
    Cmd.GovernanceVerifyPollCmdArgs
      { eon = BabbageEraOnwardsBabbage
      , pollFile
      , txFile
      , mOutFile
      }

runLegacyGovernanceMIRCertificatePayStakeAddrs
  :: EraInEon ShelleyToBabbageEra
  -> L.MIRPot
  -> [StakeAddress]
  -- ^ Stake addresses
  -> [Lovelace]
  -- ^ Corresponding reward amounts (same length)
  -> File () Out
  -> ExceptT GovernanceCmdError IO ()
runLegacyGovernanceMIRCertificatePayStakeAddrs (EraInEon w) =
  runGovernanceMIRCertificatePayStakeAddrs w

runLegacyGovernanceCreateMirCertificateTransferToTreasuryCmd
  :: EraInEon ShelleyToBabbageEra
  -> Lovelace
  -> File () Out
  -> ExceptT GovernanceCmdError IO ()
runLegacyGovernanceCreateMirCertificateTransferToTreasuryCmd (EraInEon w) =
  runGovernanceCreateMirCertificateTransferToTreasuryCmd w

runLegacyGovernanceCreateMirCertificateTransferToReservesCmd
  :: EraInEon ShelleyToBabbageEra
  -> Lovelace
  -> File () Out
  -> ExceptT GovernanceCmdError IO ()
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
  -> ExceptT GovernanceCmdError IO ()
runLegacyGovernanceUpdateProposal upFile eNo genVerKeyFiles upPprams mCostModelFp = do
  finalUpPprams <- case mCostModelFp of
    Nothing -> return upPprams
    Just fp -> do
      costModelsBs <- handleIOExceptT (GovernanceCmdCostModelReadError . FileIOError fp) $ LB.readFile fp

      cModels <-
        pure (eitherDecode costModelsBs)
          & onLeft (left . GovernanceCmdCostModelsJsonDecodeErr fp . Text.pack)

      let costModels = fromAlonzoCostModels cModels

      when (null costModels) $ left (GovernanceCmdEmptyCostModel fp)

      return $ upPprams{protocolUpdateCostModels = costModels}

  when (finalUpPprams == mempty) $ left GovernanceCmdEmptyUpdateProposalError

  genVKeys <-
    sequence
      [ firstExceptT GovernanceCmdTextEnvReadError . newExceptT $
          readFileTextEnvelope (AsVerificationKey AsGenesisKey) vkeyFile
      | vkeyFile <- genVerKeyFiles
      ]
  let genKeyHashes = fmap verificationKeyHash genVKeys
      upProp = makeShelleyUpdateProposal finalUpPprams genKeyHashes eNo

  firstExceptT GovernanceCmdTextEnvWriteError . newExceptT $
    writeLazyByteStringFile upFile $
      textEnvelopeToJSON Nothing upProp
