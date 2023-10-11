{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Legacy.Run.Governance
  ( runLegacyGovernanceCmds
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Run.Governance
import           Cardano.CLI.EraBased.Run.Governance.GenesisKeyDelegationCertificate
                   (runGovernanceGenesisKeyDelegationCertificate)
import           Cardano.CLI.EraBased.Run.Governance.Poll
import           Cardano.CLI.Legacy.Commands.Governance
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.GovernanceCmdError

import           Control.Monad
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
import           Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as LB
import           Data.Function ((&))
import qualified Data.Text as Text

runLegacyGovernanceCmds :: LegacyGovernanceCmds -> ExceptT GovernanceCmdError IO ()
runLegacyGovernanceCmds = \case
  GovernanceMIRPayStakeAddressesCertificate anyEra mirpot vKeys rewards out ->
    runLegacyGovernanceMIRCertificatePayStakeAddrs anyEra mirpot vKeys rewards out
  GovernanceMIRTransfer anyEra amt out direction -> do
    runLegacyGovernanceMIRCertificateTransfer anyEra amt out direction
  GovernanceGenesisKeyDelegationCertificate (EraInEon sbe) genVk genDelegVk vrfVk out ->
    runGovernanceGenesisKeyDelegationCertificate sbe genVk genDelegVk vrfVk out
  GovernanceUpdateProposal out eNo genVKeys ppUp mCostModelFp ->
    runLegacyGovernanceUpdateProposal out eNo genVKeys ppUp mCostModelFp
  GovernanceCreatePoll prompt choices nonce out ->
    runGovernanceCreatePoll BabbageEraOnwardsBabbage  prompt choices nonce out
  GovernanceAnswerPoll poll ix mOutFile ->
    runGovernanceAnswerPoll BabbageEraOnwardsBabbage poll ix mOutFile
  GovernanceVerifyPoll poll metadata mOutFile ->
    runGovernanceVerifyPoll BabbageEraOnwardsBabbage poll metadata mOutFile


runLegacyGovernanceMIRCertificatePayStakeAddrs
  :: EraInEon ShelleyToBabbageEra
  -> Ledger.MIRPot
  -> [StakeAddress] -- ^ Stake addresses
  -> [Lovelace]     -- ^ Corresponding reward amounts (same length)
  -> File () Out
  -> ExceptT GovernanceCmdError IO ()
runLegacyGovernanceMIRCertificatePayStakeAddrs (EraInEon w) =
  runGovernanceMIRCertificatePayStakeAddrs w

runLegacyGovernanceMIRCertificateTransfer
  :: EraInEon ShelleyToBabbageEra
  -> Lovelace
  -> File () Out
  -> TransferDirection
  -> ExceptT GovernanceCmdError IO ()
runLegacyGovernanceMIRCertificateTransfer (EraInEon w) =
  runGovernanceMIRCertificateTransfer w

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
