{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Type.Error.TxValidationError
  ( TxAuxScriptsValidationError (..)
  , TxGovDuplicateVotes (..)
  , validateScriptSupportedInEra
  , validateTxAuxScripts
  , validateRequiredSigners
  , validateTxReturnCollateral
  , validateTxScriptValidity
  , validateTxTotalCollateral
  , validateTxValidityLowerBound
  , validateTxCurrentTreasuryValue
  , validateTxTreasuryDonation
  )
where

import Cardano.Api
import Cardano.Api.Experimental
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.Orphan ()
import Cardano.CLI.Type.Common

import Prelude

import Data.Bifunctor (first)
import Prettyprinter (viaShow)

data ScriptLanguageValidationError
  = ScriptLanguageValidationError AnyScriptLanguage AnyCardanoEra
  deriving Show

instance Error ScriptLanguageValidationError where
  prettyError = \case
    ScriptLanguageValidationError lang era ->
      "The script language "
        <> pshow lang
        <> " is not supported in the "
        <> pretty era
        <> " era."

validateScriptSupportedInEra
  :: IsEra era
  => ScriptInAnyLang
  -> Either ScriptLanguageValidationError (ScriptInEra era)
validateScriptSupportedInEra script@(ScriptInAnyLang lang _) =
  let era = convert useEra
   in case toScriptInEra era script of
        Nothing ->
          Left $
            ScriptLanguageValidationError
              (AnyScriptLanguage lang)
              (anyCardanoEra $ toCardanoEra era)
        Just script' -> pure script'

validateTxTotalCollateral
  :: IsEra era
  => Maybe Lovelace
  -> TxTotalCollateral era
validateTxTotalCollateral Nothing = TxTotalCollateralNone
validateTxTotalCollateral (Just coll) = do
  TxTotalCollateral (convert useEra) coll

validateTxCurrentTreasuryValue
  :: forall era
   . Exp.IsEra era
  => Maybe TxCurrentTreasuryValue
  -> Maybe (Featured ConwayEraOnwards era (Maybe Lovelace))
validateTxCurrentTreasuryValue mCurrentTreasuryValue = do
  TxCurrentTreasuryValue{unTxCurrentTreasuryValue} <- mCurrentTreasuryValue
  obtainCommonConstraints (Exp.useEra @era) $ mkFeatured $ pure unTxCurrentTreasuryValue

validateTxTreasuryDonation
  :: forall era
   . Exp.IsEra era
  => Maybe TxTreasuryDonation
  -> Maybe (Featured ConwayEraOnwards era Lovelace)
validateTxTreasuryDonation mTreasuryDonation = do
  TxTreasuryDonation{unTxTreasuryDonation} <- mTreasuryDonation

  Exp.obtainCommonConstraints (Exp.useEra @era) $ mkFeatured unTxTreasuryDonation

validateTxReturnCollateral
  :: IsEra era
  => Maybe (TxOut CtxTx era)
  -> TxReturnCollateral CtxTx era
validateTxReturnCollateral Nothing = TxReturnCollateralNone
validateTxReturnCollateral (Just retColTxOut) = do
  TxReturnCollateral (convert useEra) retColTxOut

validateTxValidityLowerBound
  :: IsEra era
  => Maybe SlotNo
  -> TxValidityLowerBound era
validateTxValidityLowerBound Nothing = TxValidityNoLowerBound
validateTxValidityLowerBound (Just slot) = do
  TxValidityLowerBound (convert useEra) slot

data TxAuxScriptsValidationError
  = TxAuxScriptsLanguageError ScriptLanguageValidationError
  deriving Show

instance Error TxAuxScriptsValidationError where
  prettyError (TxAuxScriptsLanguageError e) =
    "Transaction auxiliary scripts error: " <> prettyError e

validateTxAuxScripts
  :: IsEra era
  => [ScriptInAnyLang]
  -> Either TxAuxScriptsValidationError (TxAuxScripts era)
validateTxAuxScripts [] = return TxAuxScriptsNone
validateTxAuxScripts scripts = do
  scriptsInEra <-
    mapM (first TxAuxScriptsLanguageError . validateScriptSupportedInEra) scripts
  pure $ TxAuxScripts (convert useEra) scriptsInEra

validateRequiredSigners
  :: IsEra era
  => [Hash PaymentKey]
  -> TxExtraKeyWitnesses era
validateRequiredSigners [] = TxExtraKeyWitnessesNone
validateRequiredSigners reqSigs = do
  TxExtraKeyWitnesses (convert useEra) reqSigs

validateTxScriptValidity
  :: IsEra era
  => Maybe ScriptValidity
  -> TxScriptValidity era
validateTxScriptValidity Nothing = TxScriptValidityNone
validateTxScriptValidity (Just scriptValidity) = do
  TxScriptValidity (convert useEra) scriptValidity

newtype TxGovDuplicateVotes era
  = TxGovDuplicateVotes (VotesMergingConflict era)

instance Error (TxGovDuplicateVotes era) where
  prettyError (TxGovDuplicateVotes (VotesMergingConflict (_voter, actionIds))) =
    "Trying to merge votes with similar action identifiers: "
      <> viaShow actionIds
      <> ". This would cause ignoring some of the votes, so not proceeding."
