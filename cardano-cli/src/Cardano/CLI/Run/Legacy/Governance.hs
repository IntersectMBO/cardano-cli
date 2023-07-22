{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Run.Legacy.Governance
  ( ShelleyGovernanceCmdError
  , renderShelleyGovernanceError
  , runGovernanceCmd
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley
import qualified Cardano.Api.Shelley as Api

import           Cardano.Binary (DecoderError)
import           Cardano.CLI.Commands.Governance
import           Cardano.CLI.EraBased.Governance
import           Cardano.CLI.Run.Legacy.Read (CddlError, fileOrPipe, readFileTx)
import           Cardano.CLI.Types.Governance
import qualified Cardano.CLI.Types.Governance as Cli
import           Cardano.CLI.Types.Legacy

import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, left, newExceptT,
                   onLeft)
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
import           Formatting (build, sformat)
import qualified System.IO as IO
import           System.IO (stderr, stdin, stdout)

data ShelleyGovernanceCmdError
  = ShelleyGovernanceCmdTextEnvReadError !(FileError TextEnvelopeError)
  | ShelleyGovernanceCmdCddlError !CddlError
  | ShelleyGovernanceCmdKeyReadError !(FileError InputDecodeError)
  | ShelleyGovernanceCmdCostModelReadError !(FileError ())
  | ShelleyGovernanceCmdTextEnvWriteError !(FileError ())
  | ShelleyGovernanceCmdEmptyUpdateProposalError
  | ShelleyGovernanceCmdMIRCertificateKeyRewardMistmach
      !FilePath
      !Int
      -- ^ Number of stake verification keys
      !Int
      -- ^ Number of reward amounts
  | ShelleyGovernanceCmdCostModelsJsonDecodeErr !FilePath !Text
  | ShelleyGovernanceCmdEmptyCostModel !FilePath
  | ShelleyGovernanceCmdUnexpectedKeyType
      ![TextEnvelopeType]
      -- ^ Expected key types
  | ShelleyGovernanceCmdPollOutOfBoundAnswer
      !Int
      -- ^ Maximum answer index
  | ShelleyGovernanceCmdPollInvalidChoice
  | ShelleyGovernanceCmdDecoderError !DecoderError
  | ShelleyGovernanceCmdVerifyPollError !GovernancePollError
  | ShelleyGovernanceCmdWriteFileError !(FileError ())
  deriving Show

renderShelleyGovernanceError :: ShelleyGovernanceCmdError -> Text
renderShelleyGovernanceError = \case
  ShelleyGovernanceCmdTextEnvReadError fileErr -> Text.pack (displayError fileErr)
  ShelleyGovernanceCmdCddlError cddlErr -> Text.pack (displayError cddlErr)
  ShelleyGovernanceCmdKeyReadError fileErr -> Text.pack (displayError fileErr)
  ShelleyGovernanceCmdTextEnvWriteError fileErr -> Text.pack (displayError fileErr)
  -- TODO: The equality check is still not working for empty update proposals.
  ShelleyGovernanceCmdEmptyUpdateProposalError ->
    "Empty update proposals are not allowed"
  ShelleyGovernanceCmdMIRCertificateKeyRewardMistmach fp numVKeys numRwdAmts ->
      "Error creating the MIR certificate at: " <> textShow fp
      <> " The number of staking keys: " <> textShow numVKeys
      <> " and the number of reward amounts: " <> textShow numRwdAmts
      <> " are not equivalent."
  ShelleyGovernanceCmdCostModelsJsonDecodeErr fp err' ->
    "Error decoding cost model: " <> err' <> " at: " <> Text.pack fp
  ShelleyGovernanceCmdEmptyCostModel fp ->
    "The decoded cost model was empty at: " <> Text.pack fp
  ShelleyGovernanceCmdCostModelReadError err' ->
    "Error reading the cost model: " <> Text.pack (displayError err')
  ShelleyGovernanceCmdUnexpectedKeyType expected ->
    "Unexpected poll key type; expected one of: "
    <> Text.intercalate ", " (textShow <$> expected)
  ShelleyGovernanceCmdPollOutOfBoundAnswer nMax ->
    "Poll answer out of bounds. Choices are between 0 and " <> textShow nMax
  ShelleyGovernanceCmdPollInvalidChoice ->
    "Invalid choice. Please choose from the available answers."
  ShelleyGovernanceCmdDecoderError decoderError ->
    "Unable to decode metadata: " <> sformat build decoderError
  ShelleyGovernanceCmdVerifyPollError pollError ->
    renderGovernancePollError pollError
  ShelleyGovernanceCmdWriteFileError fileErr -> Text.pack (displayError fileErr)

runGovernanceCmd :: GovernanceCmd -> ExceptT GovernanceCmdError IO ()
runGovernanceCmd = \case
  GovernanceVoteCmd (CreateVoteCmd (ConwayVote voteChoice voteType govActTcIn voteStakeCred sbe fp)) ->
    runGovernanceCreateVoteCmd sbe voteChoice voteType govActTcIn voteStakeCred fp
  GovernanceActionCmd (CreateConstitution (Cli.NewConstitution sbe deposit voteStakeCred newconstitution fp)) ->
    runGovernanceNewConstitutionCmd sbe deposit voteStakeCred newconstitution fp
  GovernanceUpdateProposal out eNo genVKeys ppUp mCostModelFp ->
    runGovernanceUpdateProposal out eNo genVKeys ppUp mCostModelFp
  GovernanceCreatePoll prompt choices nonce out ->
    runGovernanceCreatePoll prompt choices nonce out
  GovernanceAnswerPoll poll ix mOutFile ->
    runGovernanceAnswerPoll poll ix mOutFile
  GovernanceVerifyPoll poll metadata mOutFile ->
    runGovernanceVerifyPoll poll metadata mOutFile

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
