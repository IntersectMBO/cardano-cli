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
import Cardano.Api.Experimental.Plutus qualified as Exp.Plutus

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
  => [(TxIn, Maybe AnySpendScript)]
  -> CIO e [(TxIn, Exp.AnyWitness (LedgerEra era))]
readSpendScriptWitnesses =
  mapM (\(txin, mWit) -> (txin,) <$> readSpendScriptWitness mWit)

readSpendScriptWitness
  :: forall era e
   . IsEra era => Maybe AnySpendScript -> CIO e (Exp.AnyWitness (LedgerEra era))
readSpendScriptWitness Nothing = return Exp.AnyKeyWitnessPlaceholder
readSpendScriptWitness (Just spendScriptReq) =
  case spendScriptReq of
    AnySpendScriptSimple simpleReq ->
      case simpleReq of
        OnDiskSimpleScript simpleFp -> do
          let sFp = unFile simpleFp
          Exp.AnySimpleScriptWitness . Exp.SScript <$> readFileSimpleScript sFp (useEra @era)
        ReferenceSimpleScript refTxIn ->
          return $ Exp.AnySimpleScriptWitness $ Exp.SReferenceScript refTxIn
    AnySpendScriptPlutus plutusReq ->
      case plutusReq of
        OnDiskPlutusSpendingScript plutusScriptFp mScriptDatum redeemerFile execUnits -> do
          anyScript <-
            readFilePlutusScript @_ @era (unFile plutusScriptFp)
          case anyScript of
            Exp.Plutus.AnyPlutusScript script -> do
              redeemer <-
                fromExceptTCli $
                  readScriptDataOrFile redeemerFile
              let lang = Exp.Plutus.plutusScriptInEraSLanguage script
              mDatum <- handlePotentialScriptDatum mScriptDatum lang
              let pScript = Exp.Plutus.PScript script
                  plutusScriptWitness = Exp.Plutus.PlutusScriptWitness lang pScript mDatum redeemer execUnits
              return $
                Exp.AnyPlutusScriptWitness $
                  Exp.AnyPlutusSpendingScriptWitness $
                    Exp.createPlutusSpendingScriptWitness lang plutusScriptWitness
        ReferencePlutusSpendingScript refTxIn (AnySLanguage lang) mScriptDatum redeemerFile execUnits -> do
          let pRefScript = Exp.Plutus.PReferenceScript refTxIn
          redeemer <-
            fromExceptTCli $ readScriptDataOrFile redeemerFile
          mDatum <- handlePotentialScriptDatum mScriptDatum lang
          let plutusScriptWitness = Exp.Plutus.PlutusScriptWitness lang pRefScript mDatum redeemer execUnits
          return $
            Exp.AnyPlutusScriptWitness $
              Exp.AnyPlutusSpendingScriptWitness $
                Exp.createPlutusSpendingScriptWitness lang plutusScriptWitness

handlePotentialScriptDatum
  :: ScriptDatumOrFileSpending
  -> L.SLanguage lang
  -> CIO e (Exp.Plutus.PlutusScriptDatum lang Exp.Plutus.SpendingScript)
handlePotentialScriptDatum InlineDatum _ = return Exp.Plutus.InlineDatum
handlePotentialScriptDatum (PotentialDatum (Just sDatFp)) lang = do
  d <- fromExceptTCli $ readScriptDataOrFile sDatFp
  return $
    Exp.Plutus.SpendingScriptDatum
      ( case lang of
          L.SPlutusV1 -> d
          L.SPlutusV2 -> d
          L.SPlutusV3 -> Just d
          L.SPlutusV4 -> Just d
      )
handlePotentialScriptDatum (PotentialDatum Nothing) lang =
  case lang of
    L.SPlutusV1 ->
      throwCliError @String
        "handlePotentialScriptDatum: You must provide a script datum for Plutus V1 scripts."
    L.SPlutusV2 ->
      throwCliError @String
        "handlePotentialScriptDatum: You must provide a script datum for Plutus V2 scripts."
    L.SPlutusV3 -> return Exp.Plutus.NoScriptDatum
    L.SPlutusV4 -> return Exp.Plutus.NoScriptDatum
