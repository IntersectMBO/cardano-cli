{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Legacy.Run.Governance
  ( runLegacyGovernanceCmds
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley

import qualified Cardano.CLI.EraBased.Commands.Governance.Poll as Cmd
import           Cardano.CLI.EraBased.Run.Governance
import           Cardano.CLI.EraBased.Run.Governance.GenesisKeyDelegationCertificate
                   (runGovernanceGenesisKeyDelegationCertificate)
import           Cardano.CLI.EraBased.Run.Governance.Poll
import           Cardano.CLI.Legacy.Commands.Governance
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.GovernanceCmdError

import           Control.Monad
import           Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as LB
import           Data.Function ((&))
import           Data.Text (Text)
import qualified Data.Text as Text

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

runLegacyGovernanceCreatePoll :: ()
  => Text
  -> [Text]
  -> Maybe Word
  -> File GovernancePoll Out
  -> ExceptT GovernanceCmdError IO ()
runLegacyGovernanceCreatePoll prompt choices nonce outFile =
  runGovernanceCreatePollCmd
    Cmd.GovernanceCreatePollCmdArgs
      { eon     = BabbageEraOnwardsBabbage
      , prompt
      , choices
      , nonce
      , outFile
      }

runLegacyGovernanceAnswerPoll :: ()
  => File GovernancePoll In
  -> Maybe Word
  -> Maybe (File () Out)
  -> ExceptT GovernanceCmdError IO ()
runLegacyGovernanceAnswerPoll pollFile answerIndex mOutFile =
  runGovernanceAnswerPollCmd
    Cmd.GovernanceAnswerPollCmdArgs
      { eon         = BabbageEraOnwardsBabbage
      , pollFile
      , answerIndex
      , mOutFile
      }

runLegacyGovernanceVerifyPoll :: ()
  => File GovernancePoll In
  -> File (Tx ()) In
  -> Maybe (File () Out)
  -> ExceptT GovernanceCmdError IO ()
runLegacyGovernanceVerifyPoll pollFile txFile mOutFile =
  runGovernanceVerifyPollCmd
    Cmd.GovernanceVerifyPollCmdArgs
      { eon       = BabbageEraOnwardsBabbage
      , pollFile
      , txFile
      , mOutFile
      }

runLegacyGovernanceMIRCertificatePayStakeAddrs
  :: EraInEon ShelleyToBabbageEra
  -> L.MIRPot
  -> [StakeAddress] -- ^ Stake addresses
  -> [L.Coin]     -- ^ Corresponding reward amounts (same length)
  -> File () Out
  -> ExceptT GovernanceCmdError IO ()
runLegacyGovernanceMIRCertificatePayStakeAddrs (EraInEon w) =
  runGovernanceMIRCertificatePayStakeAddrs w

runLegacyGovernanceCreateMirCertificateTransferToTreasuryCmd
  :: EraInEon ShelleyToBabbageEra
  -> L.Coin
  -> File () Out
  -> ExceptT GovernanceCmdError IO ()
runLegacyGovernanceCreateMirCertificateTransferToTreasuryCmd (EraInEon w) =
  runGovernanceCreateMirCertificateTransferToTreasuryCmd w

runLegacyGovernanceCreateMirCertificateTransferToReservesCmd
  :: EraInEon ShelleyToBabbageEra
  -> L.Coin
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
  -> Maybe FilePath -- ^ Cost models file path
  -> ExceptT GovernanceCmdError IO ()
runLegacyGovernanceUpdateProposal upFile eNo genVerKeyFiles upPprams mCostModelFp = do
  finalUpPprams <- case mCostModelFp of
    Nothing -> return upPprams
    Just fp -> do
      costModelsBs <- handleIOExceptT (GovernanceCmdCostModelReadError . FileIOError fp) $ LB.readFile fp

      cModels <- pure (eitherDecode costModelsBs)
        & onLeft (left . GovernanceCmdCostModelsJsonDecodeErr fp . Text.pack)

      let costModels = fromAlonzoCostModels cModels

      when (null costModels) $ left (GovernanceCmdEmptyCostModel fp)

      return $ upPprams {protocolUpdateCostModels = costModels}

  when (finalUpPprams == mempty) $ left GovernanceCmdEmptyUpdateProposalError

  genVKeys <- sequence
    [ firstExceptT GovernanceCmdTextEnvReadError . newExceptT $ readFileTextEnvelope (AsVerificationKey AsGenesisKey) vkeyFile
    | vkeyFile <- genVerKeyFiles
    ]
  let genKeyHashes = fmap verificationKeyHash genVKeys
      upProp = makeShelleyUpdateProposal finalUpPprams genKeyHashes eNo

  firstExceptT GovernanceCmdTextEnvWriteError . newExceptT
    $ writeLazyByteStringFile upFile $ textEnvelopeToJSON Nothing upProp
