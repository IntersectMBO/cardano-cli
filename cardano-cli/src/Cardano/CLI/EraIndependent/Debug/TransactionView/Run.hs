{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
        IncompleteTxBody (sbe :: ShelleyBasedEra era) (Exp.UnsignedTx ledgerTx) <-
          fromEitherIOCli $ readFileTxBody txbodyFile
        era <- fromEitherCli (Exp.sbeToEra sbe)
        -- Why are we differentiating between a transaction body and a transaction?
        -- In the case of a transaction body, we /could/ simply call @addWitnesses []@
        -- to get a transaction which would allow us to reuse friendlyTxBS. However,
        -- this would mean that we'd have an empty list of witnesses mentioned in the output, which
        -- is arguably not part of the transaction body.
        let unsignedTx :: Exp.UnsignedTx (Exp.LedgerEra era)
            unsignedTx = Exp.obtainCommonConstraints era $ Exp.UnsignedTx ledgerTx
        fromEitherIOCli @(FileError ()) $
          friendlyTxBody outputFormat mOutFile era unsignedTx
      InputTxFile (File txFilePath) -> do
        txFile <- liftIO $ fileOrPipe txFilePath
        InAnyShelleyBasedEra (sbe :: ShelleyBasedEra era) tx <- fromEitherIOCli (readFileTx txFile)
        era <- fromEitherCli (Exp.sbeToEra sbe)
        let ShelleyTx _ ledgerTx = tx
            signedTx :: Exp.SignedTx era
            signedTx = Exp.obtainCommonConstraints era $ Exp.SignedTx ledgerTx
        fromEitherIOCli @(FileError ()) $
          friendlyTx outputFormat mOutFile era signedTx
