{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Legacy.Run.Governance
  ( runGovernanceCmds
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley
import qualified Cardano.Api.Shelley as Api

import           Cardano.CLI.Commands.Governance
import           Cardano.CLI.EraBased.Run.Governance
import           Cardano.CLI.Legacy.Commands.Governance
import           Cardano.CLI.Read (fileOrPipe, readFileTx)
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.GovernanceCmdError
import           Cardano.CLI.Types.Governance
import qualified Cardano.CLI.Types.Governance as Cli
import           Cardano.CLI.Types.Key (VerificationKeyOrHashOrFile,
                   readVerificationKeyOrHashOrFile, readVerificationKeyOrHashOrTextEnvFile)

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
import           Data.Aeson (eitherDecode)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LB
import           Data.Function ((&))
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import qualified System.IO as IO
import           System.IO (stderr, stdin, stdout)

runGovernanceCmds :: LegacyGovernanceCmds -> ExceptT GovernanceCmdError IO ()
runGovernanceCmds = \case
  GovernanceVoteCmd (CreateVoteCmd (ConwayVote voteChoice sbe fp)) ->
    runGovernanceCreateVoteCmd sbe voteChoice  fp
  GovernanceActionCmd (CreateConstitution (Cli.NewConstitution network sbe deposit voteStakeCred mPrevGovActId propAnchor newconstitution fp)) ->
    runGovernanceNewConstitutionCmd network sbe deposit voteStakeCred mPrevGovActId propAnchor newconstitution fp
  GovernanceMIRPayStakeAddressesCertificate (AnyShelleyToBabbageEra w) mirpot vKeys rewards out ->
    runGovernanceMIRCertificatePayStakeAddrs w mirpot vKeys rewards out
  GovernanceMIRTransfer (AnyShelleyToBabbageEra w) amt out direction -> do
    runGovernanceMIRCertificateTransfer w amt out direction
  GovernanceGenesisKeyDelegationCertificate sbe genVk genDelegVk vrfVk out ->
    runGovernanceGenesisKeyDelegationCertificate sbe genVk genDelegVk vrfVk out
  GovernanceUpdateProposal out eNo genVKeys ppUp mCostModelFp ->
    runGovernanceUpdateProposal out eNo genVKeys ppUp mCostModelFp
  GovernanceCreatePoll prompt choices nonce out ->
    runGovernanceCreatePoll prompt choices nonce out
  GovernanceAnswerPoll poll ix mOutFile ->
    runGovernanceAnswerPoll poll ix mOutFile
  GovernanceVerifyPoll poll metadata mOutFile ->
    runGovernanceVerifyPoll poll metadata mOutFile

runGovernanceGenesisKeyDelegationCertificate
  :: AnyShelleyBasedEra
  -> VerificationKeyOrHashOrFile GenesisKey
  -> VerificationKeyOrHashOrFile GenesisDelegateKey
  -> VerificationKeyOrHashOrFile VrfKey
  -> File () Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceGenesisKeyDelegationCertificate (AnyShelleyBasedEra sbe)
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

runGovernanceUpdateProposal
  :: File () Out
  -> EpochNo
  -> [VerificationKeyFile In]
  -- ^ Genesis verification keys
  -> ProtocolParametersUpdate
  -> Maybe FilePath -- ^ Cost models file path
  -> ExceptT GovernanceCmdError IO ()
runGovernanceUpdateProposal upFile eNo genVerKeyFiles upPprams mCostModelFp = do
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

runGovernanceCreatePoll
  :: Text
  -> [Text]
  -> Maybe Word
  -> File GovernancePoll Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceCreatePoll govPollQuestion govPollAnswers govPollNonce out = do
  let poll = GovernancePoll{ govPollQuestion, govPollAnswers, govPollNonce }

  let description = fromString $ "An on-chain poll for SPOs: " <> Text.unpack govPollQuestion
  firstExceptT GovernanceCmdTextEnvWriteError . newExceptT $
    writeFileTextEnvelope out (Just description) poll

  let metadata = asTxMetadata poll
        & metadataToJson TxMetadataJsonDetailedSchema

  let outPath = unFile out & Text.encodeUtf8 . Text.pack

  liftIO $ do
    BSC.hPutStrLn stderr $ mconcat
      [ "Poll created successfully.\n"
      , "Please submit a transaction using the resulting metadata.\n"
      ]
    BSC.hPutStrLn stdout (prettyPrintJSON metadata)
    BSC.hPutStrLn stderr $ mconcat
      [ "\n"
      , "Hint (1): Use '--json-metadata-detailed-schema' and '--metadata-json-file' "
      , "from the build or build-raw commands.\n"
      , "Hint (2): You can redirect the standard output of this command to a JSON "
      , "file to capture metadata.\n\n"
      , "Note: A serialized version of the poll suitable for sharing with "
      , "participants has been generated at '" <> outPath <> "'."
      ]

runGovernanceAnswerPoll
  :: File GovernancePoll In
  -> Maybe Word -- ^ Answer index
  -> Maybe (File () Out) -- ^ Output file
  -> ExceptT GovernanceCmdError IO ()
runGovernanceAnswerPoll pollFile maybeChoice mOutFile = do
  poll <- firstExceptT GovernanceCmdTextEnvReadError . newExceptT $
    readFileTextEnvelope AsGovernancePoll pollFile

  choice <- case maybeChoice of
    Nothing -> do
      askInteractively poll
    Just ix -> do
      validateChoice poll ix
      liftIO $ BSC.hPutStrLn stderr $ Text.encodeUtf8 $ Text.intercalate "\n"
        [ govPollQuestion poll
        , "→ " <> (govPollAnswers poll !! fromIntegral ix)
        , ""
        ]
      pure ix

  let pollAnswer = GovernancePollAnswer
        { govAnsPoll = hashGovernancePoll poll
        , govAnsChoice = choice
        }
  let metadata =
        metadataToJson TxMetadataJsonDetailedSchema (asTxMetadata pollAnswer)

  liftIO $ BSC.hPutStrLn stderr $ mconcat
      [ "Poll answer created successfully.\n"
      , "Please submit a transaction using the resulting metadata.\n"
      , "To be valid, the transaction must also be signed using a valid key\n"
      , "identifying your stake pool (e.g. your cold key).\n"
      ]

  lift (writeByteStringOutput mOutFile (prettyPrintJSON metadata))
    & onLeft (left . GovernanceCmdWriteFileError)

  liftIO $ BSC.hPutStrLn stderr $ mconcat
      [ "\n"
      , "Hint (1): Use '--json-metadata-detailed-schema' and '--metadata-json-file' "
      , "from the build or build-raw commands.\n"
      , "Hint (2): You can redirect the standard output of this command to a JSON "
      , "file to capture metadata."
      ]
 where
  validateChoice :: GovernancePoll -> Word -> ExceptT GovernanceCmdError IO ()
  validateChoice GovernancePoll{govPollAnswers} ix = do
    let maxAnswerIndex = length govPollAnswers - 1
    when (fromIntegral ix > maxAnswerIndex) $ left $
      GovernanceCmdPollOutOfBoundAnswer maxAnswerIndex

  askInteractively :: GovernancePoll -> ExceptT GovernanceCmdError IO Word
  askInteractively poll@GovernancePoll{govPollQuestion, govPollAnswers} = do
    liftIO $ BSC.hPutStrLn stderr $ Text.encodeUtf8 $ Text.intercalate "\n"
      ( govPollQuestion
      : [ "[" <> textShow ix <> "] " <> answer
        | (ix :: Int, answer) <- zip [0..] govPollAnswers
        ]
      )
    liftIO $ BSC.hPutStrLn stderr ""
    liftIO $ BSC.hPutStr stderr "Please indicate an answer (by index): "
    txt <- liftIO $ Text.hGetLine stdin
    liftIO $ BSC.hPutStrLn stderr ""
    case Text.decimal txt of
      Right (choice, rest) | Text.null rest ->
        choice <$ validateChoice poll choice
      _ ->
        left GovernanceCmdPollInvalidChoice

runGovernanceVerifyPoll
  :: File GovernancePoll In
  -> File (Api.Tx ()) In
  -> Maybe (File () Out) -- ^ Output file
  -> ExceptT GovernanceCmdError IO ()
runGovernanceVerifyPoll pollFile txFile mOutFile = do
  poll <- firstExceptT GovernanceCmdTextEnvReadError . newExceptT $
    readFileTextEnvelope AsGovernancePoll pollFile

  txFileOrPipe <- liftIO $ fileOrPipe (unFile txFile)
  tx <- firstExceptT GovernanceCmdCddlError . newExceptT $
    readFileTx txFileOrPipe

  signatories <- firstExceptT GovernanceCmdVerifyPollError . newExceptT $ pure $
    verifyPollAnswer poll tx

  liftIO $ IO.hPutStrLn stderr $ "Found valid poll answer with " <> show (length signatories) <> " signatories"

  lift (writeByteStringOutput mOutFile (prettyPrintJSON signatories))
    & onLeft (left . GovernanceCmdWriteFileError)
