{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Types.Errors.TxValidationError
  ( TxAuxScriptsValidationError (..)
  , TxGovDuplicateVotes (..)
  , TxNotSupportedInEraValidationError (..)
  , validateScriptSupportedInEra
  , validateTxAuxScripts
  , validateRequiredSigners
  , validateTxReturnCollateral
  , validateTxScriptValidity
  , validateTxTotalCollateral
  , validateTxValidityLowerBound
  , validateUpdateProposalFile
  , validateTxCurrentTreasuryValue
  , validateTxTreasuryDonation
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Common

import           Prelude

import           Data.Bifunctor (first)
import qualified Data.Text as T
import           Prettyprinter (viaShow)

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
  :: ShelleyBasedEra era
  -> ScriptInAnyLang
  -> Either ScriptLanguageValidationError (ScriptInEra era)
validateScriptSupportedInEra era script@(ScriptInAnyLang lang _) =
  case toScriptInEra era script of
    Nothing ->
      Left $
        ScriptLanguageValidationError
          (AnyScriptLanguage lang)
          (anyCardanoEra $ toCardanoEra era)
    Just script' -> pure script'

data TxNotSupportedInEraValidationError era
  = -- | First argument is the kind of data that is not supported.
    -- Second argument is the era that doesn't support the data.
    TxNotSupportedInAnyCardanoEraValidationError T.Text AnyCardanoEra
  | -- | First argument is the kind of data that is not supported.
    -- Second argument is the Shelley era that doesn't support the data.
    TxNotSupportedInShelleyBasedEraValidationError T.Text (ShelleyBasedEra era)

instance Show (TxNotSupportedInEraValidationError era) where
  show =
    \case
      TxNotSupportedInAnyCardanoEraValidationError a cEra -> go a cEra
      TxNotSupportedInShelleyBasedEraValidationError a sbe -> go a sbe
   where
    go a era = show (pretty a) <> " not supported in " <> show era

instance Error (TxNotSupportedInEraValidationError era) where
  prettyError =
    \case
      TxNotSupportedInAnyCardanoEraValidationError a cEra -> go a cEra
      TxNotSupportedInShelleyBasedEraValidationError a sbe -> go a sbe
   where
    go a cEra = pretty a <+> "not supported in" <+> viaShow cEra

validateTxTotalCollateral
  :: ShelleyBasedEra era
  -> Maybe L.Coin
  -> Either (TxNotSupportedInEraValidationError era) (TxTotalCollateral era)
validateTxTotalCollateral _ Nothing = return TxTotalCollateralNone
validateTxTotalCollateral sbe (Just coll) = do
  supported <-
    conjureWitness (toCardanoEra sbe) $
      TxNotSupportedInAnyCardanoEraValidationError "Transaction collateral"
  pure $ TxTotalCollateral supported coll

validateTxCurrentTreasuryValue
  :: ()
  => ShelleyBasedEra era
  -> Maybe TxCurrentTreasuryValue
  -> Either
      (TxNotSupportedInEraValidationError era)
      (Maybe (Featured ConwayEraOnwards era (Maybe L.Coin)))
validateTxCurrentTreasuryValue sbe mCurrentTreasuryValue =
  case mCurrentTreasuryValue of
    Nothing -> Right Nothing
    Just (TxCurrentTreasuryValue{unTxCurrentTreasuryValue}) ->
      caseShelleyToBabbageOrConwayEraOnwards
        (const . Left $ TxNotSupportedInShelleyBasedEraValidationError "Current treasury value" sbe)
        (const . pure . mkFeatured $ pure unTxCurrentTreasuryValue)
        sbe

validateTxTreasuryDonation
  :: ()
  => ShelleyBasedEra era
  -> Maybe TxTreasuryDonation
  -> Either (TxNotSupportedInEraValidationError era) (Maybe (Featured ConwayEraOnwards era L.Coin))
validateTxTreasuryDonation sbe mTreasuryDonation =
  case mTreasuryDonation of
    Nothing -> Right Nothing
    Just (TxTreasuryDonation{unTxTreasuryDonation}) ->
      caseShelleyToBabbageOrConwayEraOnwards
        (const . Left $ TxNotSupportedInShelleyBasedEraValidationError "Treasury donation" sbe)
        (const . pure $ mkFeatured unTxTreasuryDonation)
        sbe

validateTxReturnCollateral
  :: ShelleyBasedEra era
  -> Maybe (TxOut CtxTx era)
  -> Either (TxNotSupportedInEraValidationError era) (TxReturnCollateral CtxTx era)
validateTxReturnCollateral _ Nothing = return TxReturnCollateralNone
validateTxReturnCollateral sbe (Just retColTxOut) = do
  supported <-
    conjureWitness (toCardanoEra sbe) $
      TxNotSupportedInAnyCardanoEraValidationError "Transaction return collateral"
  pure $ TxReturnCollateral supported retColTxOut

validateTxValidityLowerBound
  :: ShelleyBasedEra era
  -> Maybe SlotNo
  -> Either (TxNotSupportedInEraValidationError era) (TxValidityLowerBound era)
validateTxValidityLowerBound _ Nothing = return TxValidityNoLowerBound
validateTxValidityLowerBound sbe (Just slot) = do
  supported <-
    conjureWitness (toCardanoEra sbe) $
      TxNotSupportedInAnyCardanoEraValidationError "Transaction validity lower bound"
  pure $ TxValidityLowerBound supported slot

data TxAuxScriptsValidationError
  = TxAuxScriptsNotSupportedInEra AnyCardanoEra
  | TxAuxScriptsLanguageError ScriptLanguageValidationError
  deriving Show

instance Error TxAuxScriptsValidationError where
  prettyError (TxAuxScriptsNotSupportedInEra era) =
    "Transaction auxiliary scripts are not supported in " <> pretty era
  prettyError (TxAuxScriptsLanguageError e) =
    "Transaction auxiliary scripts error: " <> prettyError e

validateTxAuxScripts
  :: ShelleyBasedEra era
  -> [ScriptInAnyLang]
  -> Either TxAuxScriptsValidationError (TxAuxScripts era)
validateTxAuxScripts _ [] = return TxAuxScriptsNone
validateTxAuxScripts era scripts = do
  supported <- conjureWitness (toCardanoEra era) TxAuxScriptsNotSupportedInEra
  scriptsInEra <- mapM (first TxAuxScriptsLanguageError . validateScriptSupportedInEra era) scripts
  pure $ TxAuxScripts supported scriptsInEra

validateRequiredSigners
  :: ShelleyBasedEra era
  -> [Hash PaymentKey]
  -> Either (TxNotSupportedInEraValidationError era) (TxExtraKeyWitnesses era)
validateRequiredSigners _ [] = return TxExtraKeyWitnessesNone
validateRequiredSigners sbe reqSigs = do
  supported <-
    conjureWitness (toCardanoEra sbe) $
      TxNotSupportedInAnyCardanoEraValidationError "Transaction required signers"
  pure $ TxExtraKeyWitnesses supported reqSigs

validateTxScriptValidity
  :: ShelleyBasedEra era
  -> Maybe ScriptValidity
  -> Either (TxNotSupportedInEraValidationError era) (TxScriptValidity era)
validateTxScriptValidity _ Nothing = pure TxScriptValidityNone
validateTxScriptValidity sbe (Just scriptValidity) = do
  supported <-
    conjureWitness (toCardanoEra sbe) $
      TxNotSupportedInAnyCardanoEraValidationError "Transaction script validity"
  pure $ TxScriptValidity supported scriptValidity

-- TODO legacy. This can be deleted when legacy commands are removed.
validateUpdateProposalFile
  :: CardanoEra era
  -> Maybe UpdateProposalFile
  -> Either
      (TxNotSupportedInEraValidationError era)
      (Maybe (Featured ShelleyToBabbageEra era (Maybe UpdateProposalFile)))
validateUpdateProposalFile era = \case
  Nothing -> pure Nothing
  Just updateProposal -> do
    supported <-
      conjureWitness era $ TxNotSupportedInAnyCardanoEraValidationError "Transaction update proposal"
    pure $ Just $ Featured supported $ Just updateProposal

-- TODO make this function take a ShelleyBasedEra when the last
-- CardanoEra caller is removed (there remains only one).
conjureWitness
  :: Eon eon
  => CardanoEra era
  -- ^ era to try to conjure eon from
  -> (AnyCardanoEra -> e)
  -- ^ error wrapper function
  -> Either e (eon era)
  -- ^ eon if it includes the era, an error otherwise
conjureWitness era errF =
  maybe (cardanoEraConstraints era $ Left . errF $ AnyCardanoEra era) Right $
    forEraMaybeEon era

newtype TxGovDuplicateVotes era
  = TxGovDuplicateVotes (VotesMergingConflict era)

instance Error (TxGovDuplicateVotes era) where
  prettyError (TxGovDuplicateVotes (VotesMergingConflict (_voter, actionIds))) =
    "Trying to merge votes with similar action identifiers: "
      <> viaShow actionIds
      <> ". This would cause ignoring some of the votes, so not proceeding."
