{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraIndependent.Debug.TransactionView.Run
  ( runTransactionViewCmd
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.Compatible.Exception
  ( CIO
  , fromEitherCli
  , fromEitherIOCli
  )
import Cardano.CLI.Compatible.Json.Friendly
  ( friendlyTx
  , friendlyTxBody
  )
import Cardano.CLI.EraIndependent.Debug.TransactionView.Command
import Cardano.CLI.Read
import Cardano.CLI.Type.Common

runTransactionViewCmd
  :: ()
  => TransactionViewCmdArgs
  -> CIO e ()
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
          fromEitherIOCli $
            readFileTxBody txbodyFile
        InAnyShelleyBasedEra sbe txbody <- pure $ unIncompleteTxBody unwitnessed
        -- Why are we differentiating between a transaction body and a transaction?
        -- In the case of a transaction body, we /could/ simply call @makeSignedTransaction []@
        -- to get a transaction which would allow us to reuse friendlyTxBS. However,
        -- this would mean that we'd have an empty list of witnesses mentioned in the output, which
        -- is arguably not part of the transaction body.
        era <- fromEitherCli $ Exp.sbeToEra sbe
        fromEitherIOCli @(FileError ()) $
          friendlyTxBody outputFormat mOutFile era txbody
      InputTxFile (File txFilePath) -> do
        txFile <- liftIO $ fileOrPipe txFilePath
        InAnyShelleyBasedEra sbe tx <- fromEitherIOCli (readFileTx txFile)
        era <- fromEitherCli $ Exp.sbeToEra sbe
        fromEitherIOCli @(FileError ()) $
          friendlyTx outputFormat mOutFile era tx
