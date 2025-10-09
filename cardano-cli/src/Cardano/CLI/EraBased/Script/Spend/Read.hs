{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

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

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Spend.Type
  ( SpendScriptWitness (..)
  )
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Read

import Control.Monad

newtype CliSpendScriptWitnessError
  = CliScriptWitnessError CliScriptWitnessError
  deriving Show

instance Error CliSpendScriptWitnessError where
  prettyError = \case
    CliScriptWitnessError e -> prettyError e

readSpendScriptWitnesses
  :: IsEra era
  => [(TxIn, Maybe (ScriptRequirements TxInItem))]
  -> CIO e [(TxIn, Maybe (SpendScriptWitness era))]
readSpendScriptWitnesses =
  mapM
    ( \(txin, mSWit) -> do
        (txin,) <$> forM mSWit readSpendScriptWitness
    )

readSpendScriptWitness
  :: IsEra era => ScriptRequirements TxInItem -> CIO e (SpendScriptWitness era)
readSpendScriptWitness spendScriptReq =
  let sbe = convert useEra
   in case spendScriptReq of
        OnDiskSimpleScript simpleFp -> do
          let sFp = unFile simpleFp
          s <-
            readFileSimpleScript sFp
          case s of
            SimpleScript ss -> do
              return $
                SpendScriptWitness $
                  SimpleScriptWitness (sbeToSimpleScriptLanguageInEra sbe) $
                    SScript ss
        OnDiskPlutusScript
          (OnDiskPlutusScriptCliArgs plutusScriptFp mScriptDatum redeemerFile execUnits) -> do
            plutusScript <-
              readFilePlutusScript $
                unFile plutusScriptFp
            redeemer <-
              fromExceptTCli $
                readScriptDataOrFile redeemerFile
            case plutusScript of
              AnyPlutusScript lang script -> do
                let pScript = PScript script
                sLangSupported <-
                  fromMaybeCli
                    ( CliScriptWitnessError $
                        PlutusScriptWitnessLanguageNotSupportedInEra
                          (AnyPlutusScriptVersion lang)
                          (shelleyBasedEraConstraints sbe $ AnyShelleyBasedEra sbe)
                    )
                    $ scriptLanguageSupportedInEra sbe
                    $ PlutusScriptLanguage lang
                mDatum <- handlePotentialScriptDatum mScriptDatum
                return $
                  SpendScriptWitness $
                    PlutusScriptWitness
                      sLangSupported
                      lang
                      pScript
                      mDatum
                      redeemer
                      execUnits
        SimpleReferenceScript (SimpleRefScriptArgs refTxIn NoPolicyId) ->
          return $
            SpendScriptWitness $
              SimpleScriptWitness
                (sbeToSimpleScriptLanguageInEra sbe)
                (SReferenceScript refTxIn)
        PlutusReferenceScript
          (PlutusRefScriptCliArgs refTxIn anyPlutusScriptVersion mScriptDatum NoPolicyId redeemerFile execUnits) ->
            case anyPlutusScriptVersion of
              AnyPlutusScriptVersion lang -> do
                let pScript = PReferenceScript refTxIn
                redeemer <-
                  fromExceptTCli $ readScriptDataOrFile redeemerFile
                sLangSupported <-
                  fromMaybeCli
                    ( CliScriptWitnessError $
                        PlutusScriptWitnessLanguageNotSupportedInEra
                          (AnyPlutusScriptVersion lang)
                          (shelleyBasedEraConstraints sbe $ AnyShelleyBasedEra sbe)
                    )
                    $ scriptLanguageSupportedInEra sbe
                    $ PlutusScriptLanguage lang

                mDatum <- handlePotentialScriptDatum mScriptDatum
                return $
                  SpendScriptWitness $
                    PlutusScriptWitness
                      sLangSupported
                      lang
                      pScript
                      mDatum
                      redeemer
                      execUnits

handlePotentialScriptDatum
  :: ScriptDatumOrFileSpending
  -> CIO e (ScriptDatum WitCtxTxIn)
handlePotentialScriptDatum InlineDatum = return InlineScriptDatum
handlePotentialScriptDatum (PotentialDatum mDatum) =
  ScriptDatumForTxIn
    <$> forM mDatum (fromExceptTCli . readScriptDataOrFile)
