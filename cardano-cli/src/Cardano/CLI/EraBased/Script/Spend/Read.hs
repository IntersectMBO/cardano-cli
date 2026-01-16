{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Script.Spend.Read
  ( CliSpendScriptWitnessError
  , readSpendScriptWitness
  , readSpendScriptWitnesses
  )
where

import Cardano.Api
import Cardano.Api.Experimental hiding
  ( InlineDatum
  , PReferenceScript
  , PScript
  , PlutusScriptWitness
  , SReferenceScript
  , SScript
  , SimpleScript
  )
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.AnyScriptWitness qualified as Exp
import Cardano.Api.Experimental.Plutus qualified as Exp

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Read
import Cardano.CLI.Type.Common (AnySLanguage (..))
import Cardano.Ledger.Plutus.Language qualified as L

newtype CliSpendScriptWitnessError
  = CliScriptWitnessError CliScriptWitnessError
  deriving Show

instance Error CliSpendScriptWitnessError where
  prettyError = \case
    CliScriptWitnessError e -> prettyError e

readSpendScriptWitnesses
  :: IsEra era
  => [(TxIn, Maybe (ScriptRequirements TxInItem))]
  -> CIO e [(TxIn, Exp.AnyWitness (LedgerEra era))]
readSpendScriptWitnesses =
  mapM (\(txin, mWit) -> (txin,) <$> readSpendScriptWitness mWit)

readSpendScriptWitness
  :: forall era e
   . IsEra era => Maybe (ScriptRequirements TxInItem) -> CIO e (Exp.AnyWitness (LedgerEra era))
readSpendScriptWitness Nothing = return Exp.AnyKeyWitnessPlaceholder
readSpendScriptWitness (Just spendScriptReq) =
  case spendScriptReq of
    OnDiskSimpleScript simpleFp -> do
      let sFp = unFile simpleFp
      Exp.AnySimpleScriptWitness . Exp.SScript <$> readFileSimpleScript sFp (useEra @era)
    OnDiskPlutusScript
      (OnDiskPlutusScriptCliArgs plutusScriptFp mScriptDatum redeemerFile execUnits) -> do
        anyScript <-
          readFilePlutusScript @_ @era (unFile plutusScriptFp)
        case anyScript of
          Exp.AnyPlutusScript script -> do
            redeemer <-
              fromExceptTCli $
                readScriptDataOrFile redeemerFile
            let lang = Exp.plutusScriptInEraSLanguage script
            mDatum <- handlePotentialScriptDatum mScriptDatum lang

            let pScript = Exp.PScript script
                plutusScriptWitness = Exp.PlutusScriptWitness lang pScript mDatum redeemer execUnits
            return $
              Exp.AnyPlutusScriptWitness $
                Exp.AnyPlutusSpendingScriptWitness $
                  Exp.createPlutusSpendingScriptWitness lang plutusScriptWitness
    SimpleReferenceScript (SimpleRefScriptArgs refTxIn NoPolicyId) ->
      return $
        Exp.AnySimpleScriptWitness $
          Exp.SReferenceScript refTxIn
    PlutusReferenceScript
      (PlutusRefScriptCliArgs refTxIn (AnySLanguage lang) mScriptDatum NoPolicyId redeemerFile execUnits) -> do
        let pRefScript = Exp.PReferenceScript refTxIn
        redeemer <-
          fromExceptTCli $ readScriptDataOrFile redeemerFile

        mDatum <- handlePotentialScriptDatum mScriptDatum lang
        let plutusScriptWitness = Exp.PlutusScriptWitness lang pRefScript mDatum redeemer execUnits
        return $
          Exp.AnyPlutusScriptWitness $
            Exp.AnyPlutusSpendingScriptWitness $
              Exp.createPlutusSpendingScriptWitness lang plutusScriptWitness

handlePotentialScriptDatum
  :: ScriptDatumOrFileSpending
  -> L.SLanguage lang
  -> CIO e (Exp.PlutusScriptDatum lang Exp.SpendingScript)
handlePotentialScriptDatum InlineDatum _ = return Exp.InlineDatum
handlePotentialScriptDatum (PotentialDatum (Just sDatFp)) lang =
  case lang of
    L.SPlutusV1 -> do
      d <- fromExceptTCli $ readScriptDataOrFile sDatFp
      return $ Exp.SpendingScriptDatum d
    L.SPlutusV2 -> do
      d <- fromExceptTCli $ readScriptDataOrFile sDatFp
      return $ Exp.SpendingScriptDatum d
    L.SPlutusV3 -> do
      d <- fromExceptTCli $ readScriptDataOrFile sDatFp
      return $ Exp.SpendingScriptDatum $ Just d
    L.SPlutusV4 -> do
      d <- fromExceptTCli $ readScriptDataOrFile sDatFp
      return $ Exp.SpendingScriptDatum $ Just d
handlePotentialScriptDatum (PotentialDatum Nothing) lang =
  case lang of
    L.SPlutusV1 ->
      throwCliError @String
        "handlePotentialScriptDatum: You must provide a script datum for Plutus V1 scripts."
    L.SPlutusV2 ->
      throwCliError @String
        "handlePotentialScriptDatum: You must provide a script datum for Plutus V2 scripts."
    L.SPlutusV3 -> return Exp.NoScriptDatum
    L.SPlutusV4 -> return Exp.NoScriptDatum
