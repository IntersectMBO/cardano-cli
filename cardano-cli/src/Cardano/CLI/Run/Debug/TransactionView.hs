{-# LANGUAGE NamedFieldPuns #-}

module Cardano.CLI.Run.Debug.TransactionView
  ( runTransactionViewCmd
  )
where

import           Cardano.Api

import           Cardano.CLI.Commands.Debug.TransactionView
import           Cardano.CLI.Json.Friendly (FriendlyFormat (..), friendlyTx, friendlyTxBody)
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.TxCmdError

import           Data.Function ((&))

runTransactionViewCmd
  :: ()
  => TransactionViewCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionViewCmd
  TransactionViewCmdArgs
    { outputFormat
    , mOutFile
    , inputTxBodyOrTxFile
    } =
    case inputTxBodyOrTxFile of
      InputTxBodyFile (File txbodyFilePath) -> do
        txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
        unwitnessed <-
          firstExceptT TxCmdTextEnvCddlError . newExceptT $
            readFileTxBody txbodyFile
        InAnyShelleyBasedEra era txbody <- pure $ unIncompleteCddlTxBody unwitnessed
        -- Why are we differentiating between a transaction body and a transaction?
        -- In the case of a transaction body, we /could/ simply call @makeSignedTransaction []@
        -- to get a transaction which would allow us to reuse friendlyTxBS. However,
        -- this would mean that we'd have an empty list of witnesses mentioned in the output, which
        -- is arguably not part of the transaction body.
        firstExceptT TxCmdWriteFileError . newExceptT $
          case outputFormat of
            ViewOutputFormatYaml -> friendlyTxBody FriendlyYaml mOutFile (toCardanoEra era) txbody
            ViewOutputFormatJson -> friendlyTxBody FriendlyJson mOutFile (toCardanoEra era) txbody
      InputTxFile (File txFilePath) -> do
        txFile <- liftIO $ fileOrPipe txFilePath
        InAnyShelleyBasedEra era tx <- lift (readFileTx txFile) & onLeft (left . TxCmdTextEnvCddlError)
        firstExceptT TxCmdWriteFileError . newExceptT $
          case outputFormat of
            ViewOutputFormatYaml -> friendlyTx FriendlyYaml mOutFile (toCardanoEra era) tx
            ViewOutputFormatJson -> friendlyTx FriendlyJson mOutFile (toCardanoEra era) tx
