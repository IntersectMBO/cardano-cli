{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Run.Debug.Transaction.Echo
  ( runDebugTransactionEchoCmd
  )
where

import           Cardano.Api

import qualified Cardano.CLI.Commands.Debug.Transaction.Echo as Cmd
import           Cardano.CLI.Orphans ()
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.DebugCmdError

import           Data.Function ((&))

runDebugTransactionEchoCmd
  :: ()
  => Cmd.TransactionEchoCmdArgs
  -> ExceptT DebugCmdError IO ()
runDebugTransactionEchoCmd
  Cmd.TransactionEchoCmdArgs
    { Cmd.txOrTxBodyFile = txOrTxBody
    , Cmd.outTxFile = outTxFile
    } = do
    case txOrTxBody of
      InputTxFile (File inputTxFilePath) -> do
        inputTxFile <- liftIO $ fileOrPipe inputTxFilePath
        anyTx <- lift (readFileTx inputTxFile) & onLeft (left . DebugCmdTextEnvCddlError)

        InAnyShelleyBasedEra sbe tx <- pure anyTx

        lift (writeTxFileTextEnvelopeCddl sbe outTxFile tx)
          & onLeft (left . DebugCmdWriteFileError)
      InputTxBodyFile (File txbodyFilePath) -> do
        txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
        unwitnessed <- firstExceptT DebugCmdTextEnvCddlError . newExceptT $ readFileTxBody txbodyFile

        case unwitnessed of
          IncompleteCddlTxBody anyTxBody -> do
            InAnyShelleyBasedEra sbe txbody <- pure anyTxBody

            let tx = makeSignedTransaction [] txbody

            firstExceptT DebugCmdWriteFileError . newExceptT $
              writeLazyByteStringFile outTxFile $
                shelleyBasedEraConstraints sbe $
                  textEnvelopeToJSON Nothing tx
