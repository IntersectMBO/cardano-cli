{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Types.Errors.TxValidationError
  ( TxAuxScriptsValidationError(..)
  , TxCertificatesValidationError(..)
  , TxFeeValidationError(..)
  , TxProtocolParametersValidationError
  , TxScriptValidityValidationError(..)
  , TxUpdateProposalValidationError(..)
  , TxValidityLowerBoundValidationError(..)
  , TxValidityUpperBoundValidationError(..)
  , TxRequiredSignersValidationError
  , TxReturnCollateralValidationError(..)
  , TxTotalCollateralValidationError(..)
  , TxWithdrawalsValidationError(..)
  , validateProtocolParameters
  , validateScriptSupportedInEra
  , validateTxAuxScripts
  , validateTxCertificates
  , validateTxFee
  , validateRequiredSigners
  , validateTxReturnCollateral
  , validateTxScriptValidity
  , validateTxTotalCollateral
  , validateTxValidityUpperBound
  , validateTxValidityLowerBound
  , validateTxWithdrawals
  , validateUpdateProposalFile
  ) where

import           Cardano.Api
import           Cardano.Api.Pretty
import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Common

import           Prelude

import           Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import           Data.Maybe

data ScriptLanguageValidationError
  = ScriptLanguageValidationError AnyScriptLanguage AnyCardanoEra
  deriving Show

instance Error ScriptLanguageValidationError where
  prettyError = \case
    ScriptLanguageValidationError lang era ->
      "The script language " <> pshow lang <> " is not supported in the " <>
      pretty (renderEra era) <> " era."

validateScriptSupportedInEra
  :: ShelleyBasedEra era
  -> ScriptInAnyLang
  -> Either ScriptLanguageValidationError (ScriptInEra era)
validateScriptSupportedInEra era script@(ScriptInAnyLang lang _) =
  case toScriptInEra era script of
    Nothing -> Left $ ScriptLanguageValidationError
                        (AnyScriptLanguage lang) (anyCardanoEra $ toCardanoEra era)
    Just script' -> pure script'


data TxFeeValidationError
  = TxFeatureImplicitFeesE AnyCardanoEra -- ^ Expected an explicit fee
  | TxFeatureExplicitFeesE AnyCardanoEra -- ^ Expected an implicit fee
  deriving Show

instance Error TxFeeValidationError where
  prettyError (TxFeatureImplicitFeesE era) =
    "Implicit transaction fee not supported in " <> pretty (renderEra era)
  prettyError (TxFeatureExplicitFeesE era) =
    "Explicit transaction fee not supported in " <> pretty (renderEra era)

validateTxFee :: CardanoEra era
              -> Maybe Lovelace
              -> Either TxFeeValidationError (TxFee era)
validateTxFee era = \case
  Nothing ->
    caseByronOrShelleyBasedEra
      (pure . TxFeeImplicit)
      (const $ Left . TxFeatureImplicitFeesE $ cardanoEraConstraints era $ AnyCardanoEra era)
      era
  Just fee ->
    caseByronOrShelleyBasedEra
      (const $ Left . TxFeatureExplicitFeesE $ cardanoEraConstraints era $ AnyCardanoEra era)
      (\w -> pure (TxFeeExplicit w fee))
      era

newtype TxTotalCollateralValidationError
  = TxTotalCollateralNotSupported AnyCardanoEra
  deriving Show

instance Error TxTotalCollateralValidationError where
  prettyError (TxTotalCollateralNotSupported era) =
    "Transaction collateral not supported in " <> pretty (renderEra era)

validateTxTotalCollateral :: CardanoEra era
                          -> Maybe Lovelace
                          -> Either TxTotalCollateralValidationError (TxTotalCollateral era)
validateTxTotalCollateral _ Nothing = return TxTotalCollateralNone
validateTxTotalCollateral era (Just coll) = do
  supported <- conjureWitness era TxTotalCollateralNotSupported
  pure $ TxTotalCollateral supported coll

newtype TxReturnCollateralValidationError
  = TxReturnCollateralNotSupported AnyCardanoEra
  deriving Show

instance Error TxReturnCollateralValidationError where
  prettyError (TxReturnCollateralNotSupported era) =
    "Transaction return collateral not supported in " <> pretty (renderEra era)

validateTxReturnCollateral :: CardanoEra era
                           -> Maybe (TxOut CtxTx era)
                           -> Either TxReturnCollateralValidationError (TxReturnCollateral CtxTx era)
validateTxReturnCollateral _ Nothing = return TxReturnCollateralNone
validateTxReturnCollateral era (Just retColTxOut) = do
  supported <- conjureWitness era TxReturnCollateralNotSupported
  pure $ TxReturnCollateral supported retColTxOut

newtype TxValidityLowerBoundValidationError
  = TxValidityLowerBoundNotSupported AnyCardanoEra
  deriving Show

instance Error TxValidityLowerBoundValidationError where
  prettyError (TxValidityLowerBoundNotSupported era) =
    "Transaction validity lower bound not supported in " <> pretty (renderEra era)


validateTxValidityLowerBound :: CardanoEra era
                             -> Maybe SlotNo
                             -> Either TxValidityLowerBoundValidationError (TxValidityLowerBound era)
validateTxValidityLowerBound _ Nothing = return TxValidityNoLowerBound
validateTxValidityLowerBound era (Just slot) = do
  supported <- conjureWitness era TxValidityLowerBoundNotSupported
  pure $ TxValidityLowerBound supported slot

newtype TxValidityUpperBoundValidationError
  = TxValidityUpperBoundNotSupported AnyCardanoEra
  deriving Show

instance Error TxValidityUpperBoundValidationError where
  prettyError (TxValidityUpperBoundNotSupported era) =
    "Transaction validity upper bound must be specified in " <> pretty (renderEra era)

validateTxValidityUpperBound
  :: CardanoEra era
  -> Maybe SlotNo
  -> Either TxValidityUpperBoundValidationError (TxValidityUpperBound era)
validateTxValidityUpperBound era = \case
  Just slot -> do
    supported <- conjureWitness era TxValidityUpperBoundNotSupported
    pure $ TxValidityUpperBound supported (Just slot)
  Nothing -> do
    supported <- conjureWitness era TxValidityUpperBoundNotSupported
    pure $ TxValidityNoUpperBound supported

data TxAuxScriptsValidationError
  = TxAuxScriptsNotSupportedInEra AnyCardanoEra
  | TxAuxScriptsLanguageError ScriptLanguageValidationError
  deriving Show

instance Error TxAuxScriptsValidationError where
  prettyError (TxAuxScriptsNotSupportedInEra era) =
    "Transaction auxiliary scripts are not supported in " <> pretty (renderEra era)
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

newtype TxRequiredSignersValidationError
  = TxRequiredSignersValidationError AnyCardanoEra
  deriving Show

instance Error TxRequiredSignersValidationError where
  prettyError (TxRequiredSignersValidationError e) =
    "Transaction required signers are not supported in " <> pretty (renderEra e)

validateRequiredSigners
  :: CardanoEra era
  -> [Hash PaymentKey]
  -> Either TxRequiredSignersValidationError (TxExtraKeyWitnesses era)
validateRequiredSigners _ [] = return TxExtraKeyWitnessesNone
validateRequiredSigners era reqSigs = do
  supported <- conjureWitness era TxRequiredSignersValidationError
  pure $ TxExtraKeyWitnesses supported reqSigs

newtype TxWithdrawalsValidationError
  = TxWithdrawalsNotSupported AnyCardanoEra
  deriving Show

instance Error TxWithdrawalsValidationError where
  prettyError (TxWithdrawalsNotSupported e) =
    "Transaction withdrawals are not supported in " <> pretty (renderEra e)

validateTxWithdrawals
  :: forall era.
     CardanoEra era
  -> [(StakeAddress, Lovelace, Maybe (ScriptWitness WitCtxStake era))]
  -> Either TxWithdrawalsValidationError (TxWithdrawals BuildTx era)
validateTxWithdrawals _ [] = return TxWithdrawalsNone
validateTxWithdrawals era withdrawals = do
  supported <- conjureWitness era TxWithdrawalsNotSupported
  let convWithdrawals = map convert withdrawals
  pure $ TxWithdrawals supported convWithdrawals
 where
  convert
    :: (StakeAddress, Lovelace, Maybe (ScriptWitness WitCtxStake era))
    -> (StakeAddress, Lovelace, BuildTxWith BuildTx (Witness WitCtxStake era))
  convert (sAddr, ll, mScriptWitnessFiles) =
    case mScriptWitnessFiles of
      Just sWit -> (sAddr, ll, BuildTxWith $ ScriptWitness ScriptWitnessForStakeAddr sWit)
      Nothing   -> (sAddr, ll, BuildTxWith $ KeyWitness KeyWitnessForStakeAddr)

newtype TxCertificatesValidationError
  = TxCertificatesValidationNotSupported AnyCardanoEra
  deriving Show

instance Error TxCertificatesValidationError where
  prettyError (TxCertificatesValidationNotSupported e) =
    "Transaction certificates are not supported in " <> pretty (renderEra e)

validateTxCertificates
  :: forall era.
     CardanoEra era
  -> [(Certificate era, Maybe (ScriptWitness WitCtxStake era))]
  -> Either TxCertificatesValidationError (TxCertificates BuildTx era)
validateTxCertificates _ [] = return TxCertificatesNone
validateTxCertificates era certsAndScriptWitnesses = cardanoEraConstraints era $ do
  supported <- conjureWitness era TxCertificatesValidationNotSupported
  let certs = map fst certsAndScriptWitnesses
      reqWits = Map.fromList $ mapMaybe convert certsAndScriptWitnesses
  pure $ TxCertificates supported certs $ BuildTxWith reqWits
  where
    convert
      :: (Certificate era, Maybe (ScriptWitness WitCtxStake era))
      -> Maybe (StakeCredential, Witness WitCtxStake era)
    convert (cert, mScriptWitnessFiles) = do
      sCred <- selectStakeCredentialWitness cert
      Just $ case mScriptWitnessFiles of
        Just sWit -> (sCred, ScriptWitness ScriptWitnessForStakeAddr sWit)
        Nothing   -> (sCred, KeyWitness KeyWitnessForStakeAddr)

newtype TxProtocolParametersValidationError
  = ProtocolParametersNotSupported AnyCardanoEra
  deriving Show

instance Error TxProtocolParametersValidationError where
  prettyError (ProtocolParametersNotSupported e) =
    "Transaction protocol parameters are not supported in " <> pretty (renderEra e)

validateProtocolParameters
  :: CardanoEra era
  -> Maybe (LedgerProtocolParameters era)
  -> Either TxProtocolParametersValidationError (BuildTxWith BuildTx (Maybe (LedgerProtocolParameters era)))
validateProtocolParameters _ Nothing = return (BuildTxWith Nothing)
validateProtocolParameters era (Just pparams) = do
  _ <- conjureWitness @ShelleyBasedEra era ProtocolParametersNotSupported
  pure . BuildTxWith $ Just pparams

newtype TxUpdateProposalValidationError
  = TxUpdateProposalNotSupported AnyCardanoEra
  deriving Show

instance Error TxUpdateProposalValidationError where
  prettyError (TxUpdateProposalNotSupported e) =
    "Transaction update proposal is not supported in " <> pretty (renderEra e)

newtype TxScriptValidityValidationError
  = ScriptValidityNotSupported AnyCardanoEra
  deriving Show

instance Error TxScriptValidityValidationError where
  prettyError (ScriptValidityNotSupported e) =
    "Transaction script validity is not supported in " <> pretty (renderEra e)

validateTxScriptValidity
  :: CardanoEra era
  -> Maybe ScriptValidity
  -> Either TxScriptValidityValidationError (TxScriptValidity era)
validateTxScriptValidity _ Nothing = pure TxScriptValidityNone
validateTxScriptValidity era (Just scriptValidity) = do
  supported <- conjureWitness era ScriptValidityNotSupported
  pure $ TxScriptValidity supported scriptValidity

-- TODO legacy. This can be deleted when legacy commands are removed.
validateUpdateProposalFile
  :: CardanoEra era
  -> Maybe UpdateProposalFile
  -> Either TxUpdateProposalValidationError (Maybe (Featured ShelleyToBabbageEra era (Maybe UpdateProposalFile)))
validateUpdateProposalFile era = \case
  Nothing -> pure Nothing
  Just updateProposal -> do
    supported <- conjureWitness era TxUpdateProposalNotSupported
    pure $ Just $ Featured supported $ Just updateProposal

conjureWitness :: Eon eon
               => CardanoEra era -- ^ era to try to conjure eon from
               -> (AnyCardanoEra -> e)  -- ^ error wrapper function
               -> Either e (eon era) -- ^ eon if it includes the era, an error otherwise
conjureWitness era errF =
  maybe (cardanoEraConstraints era $ Left . errF $ AnyCardanoEra era) Right $
    forEraMaybeEon era
