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
import           Cardano.CLI.Legacy.Commands.Governance
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.GovernanceCmdError
import           Cardano.CLI.Types.Key

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
  GovernanceGenesisKeyDelegationCertificate sbe genVk genDelegVk vrfVk out ->
    runLegacyGovernanceGenesisKeyDelegationCertificate sbe genVk genDelegVk vrfVk out
  GovernanceUpdateProposal out eNo genVKeys ppUp mCostModelFp ->
    runLegacyGovernanceUpdateProposal out eNo genVKeys ppUp mCostModelFp

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

runLegacyGovernanceGenesisKeyDelegationCertificate
  :: EraInEon ShelleyBasedEra
  -> VerificationKeyOrHashOrFile GenesisKey
  -> VerificationKeyOrHashOrFile GenesisDelegateKey
  -> VerificationKeyOrHashOrFile VrfKey
  -> File () Out
  -> ExceptT GovernanceCmdError IO ()
runLegacyGovernanceGenesisKeyDelegationCertificate (EraInEon sbe)
                                             genVkOrHashOrFp
                                             genDelVkOrHashOrFp
                                             vrfVkOrHashOrFp
                                             oFp = do
  genesisVkHash <- firstExceptT GovernanceCmdKeyReadError
    . newExceptT
    $ readVerificationKeyOrHashOrTextEnvFile AsGenesisKey genVkOrHashOrFp
  genesisDelVkHash <-firstExceptT GovernanceCmdKeyReadError
    . newExceptT
    $ readVerificationKeyOrHashOrTextEnvFile AsGenesisDelegateKey genDelVkOrHashOrFp
  vrfVkHash <- firstExceptT GovernanceCmdKeyReadError
    . newExceptT
    $ readVerificationKeyOrHashOrFile AsVrfKey vrfVkOrHashOrFp

  req <- hoistEither $ createGenesisDelegationRequirements sbe genesisVkHash genesisDelVkHash vrfVkHash
  let genKeyDelegCert = makeGenesisKeyDelegationCertificate req

  firstExceptT GovernanceCmdTextEnvWriteError
    . newExceptT
    $ writeLazyByteStringFile oFp
    $ shelleyBasedEraConstraints sbe
    $ textEnvelopeToJSON (Just genKeyDelegCertDesc) genKeyDelegCert
  where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "Genesis Key Delegation Certificate"

createGenesisDelegationRequirements
  :: ShelleyBasedEra era
  -> Hash GenesisKey
  -> Hash GenesisDelegateKey
  -> Hash VrfKey
  -> Either GovernanceCmdError (GenesisKeyDelegationRequirements era)
createGenesisDelegationRequirements sbe hGen hGenDeleg hVrf =
  case sbe of
    ShelleyBasedEraShelley -> do
      return $ GenesisKeyDelegationRequirements ShelleyToBabbageEraShelley hGen hGenDeleg hVrf
    ShelleyBasedEraAllegra -> do
      return $ GenesisKeyDelegationRequirements ShelleyToBabbageEraAllegra hGen hGenDeleg hVrf
    ShelleyBasedEraMary -> do
      return $ GenesisKeyDelegationRequirements ShelleyToBabbageEraMary hGen hGenDeleg hVrf
    ShelleyBasedEraAlonzo -> do
      return $ GenesisKeyDelegationRequirements ShelleyToBabbageEraAlonzo hGen hGenDeleg hVrf
    ShelleyBasedEraBabbage -> do
      return $ GenesisKeyDelegationRequirements ShelleyToBabbageEraBabbage hGen hGenDeleg hVrf
    ShelleyBasedEraConway ->
      Left GovernanceCmdGenesisDelegationNotSupportedInConway

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
