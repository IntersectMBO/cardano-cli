{-# LANGUAGE NamedFieldPuns #-}

module Cardano.CLI.EraIndependent.Debug.TransactionView.Run
  ( runTransactionViewCmd
  )
where

import Cardano.Api

import Cardano.CLI.Compatible.Json.Friendly
  ( friendlyTx
  , friendlyTxBody
  )
import Cardano.CLI.EraIndependent.Debug.TransactionView.Command
import Cardano.CLI.Read
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.TxCmdError

import Data.Function ((&))

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
          friendlyTxBody outputFormat mOutFile era txbody
      InputTxFile (File txFilePath) -> do
        txFile <- liftIO $ fileOrPipe txFilePath
        InAnyShelleyBasedEra era tx <- lift (readFileTx txFile) & onLeft (left . TxCmdTextEnvCddlError)
        firstExceptT TxCmdWriteFileError . newExceptT $
          friendlyTx outputFormat mOutFile era tx
