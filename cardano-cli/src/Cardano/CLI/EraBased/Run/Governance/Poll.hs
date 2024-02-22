{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Run.Governance.Poll
  ( runGovernancePollCmds,
    runGovernanceCreatePollCmd,
    runGovernanceAnswerPollCmd,
    runGovernanceVerifyPollCmd
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.CLI.EraBased.Commands.Governance.Poll as Cmd
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Errors.GovernanceCmdError

import           Control.Monad
import qualified Data.ByteString.Char8 as BSC
import           Data.Function ((&))
import           Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import qualified System.IO as IO
import           System.IO (stderr, stdin, stdout)


runGovernancePollCmds :: ()
  => Cmd.GovernancePollCmds era
  -> ExceptT GovernanceCmdError IO ()
runGovernancePollCmds = \case
  Cmd.GovernanceCreatePoll args -> runGovernanceCreatePollCmd args
  Cmd.GovernanceAnswerPoll args -> runGovernanceAnswerPollCmd args
  Cmd.GovernanceVerifyPoll args -> runGovernanceVerifyPollCmd args

runGovernanceCreatePollCmd :: ()
  => Cmd.GovernanceCreatePollCmdArgs era
  -> ExceptT GovernanceCmdError IO ()
runGovernanceCreatePollCmd
    Cmd.GovernanceCreatePollCmdArgs
      { eon     = _eon
      , prompt  = govPollQuestion
      , choices = govPollAnswers
      , nonce   = govPollNonce
      , outFile = out
      } = do
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

runGovernanceAnswerPollCmd :: ()
  => Cmd.GovernanceAnswerPollCmdArgs era
  -> ExceptT GovernanceCmdError IO ()
runGovernanceAnswerPollCmd
    Cmd.GovernanceAnswerPollCmdArgs
      { eon         = _eon
      , pollFile    = pollFile
      , answerIndex = maybeChoice
      , mOutFile    = mOutFile
       } = do
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
        ixInt = fromIntegral ix
    when (ixInt < 0 || ixInt > maxAnswerIndex) $ left $
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

runGovernanceVerifyPollCmd :: ()
  => Cmd.GovernanceVerifyPollCmdArgs era
  -> ExceptT GovernanceCmdError IO ()
runGovernanceVerifyPollCmd
    Cmd.GovernanceVerifyPollCmdArgs
      { eon       = _eon
      , pollFile  = pollFile
      , txFile    = txFile
      , mOutFile  = mOutFile
      } = do
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
