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
  , TxGovDuplicateVotes(..)
  , TxProtocolParametersValidationError
  , TxScriptValidityValidationError(..)
  , TxUpdateProposalValidationError(..)
  , TxValidityLowerBoundValidationError(..)
  , TxValidityUpperBoundValidationError(..)
  , TxRequiredSignersValidationError
  , TxReturnCollateralValidationError(..)
  , TxTotalCollateralValidationError(..)
  , TxWithdrawalsValidationError(..)
  , convToTxProposalProcedures
  , convertToTxVotingProcedures
  , validateProtocolParameters
  , validateScriptSupportedInEra
  , validateTxAuxScripts
  , validateTxCertificates
  , validateTxFee
  , validateRequiredSigners
  , validateTxReturnCollateral
  , validateTxScriptValidity
  , validateTxTotalCollateral
  , validateTxValidityLowerBound
  , validateTxWithdrawals
  , validateUpdateProposalFile
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Common

import           Prelude

import           Control.Monad (foldM)
import           Data.Bifunctor (first)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.OSet.Strict as OSet
import qualified Data.Set as Set
import           Prettyprinter (viaShow)

data ScriptLanguageValidationError
  = ScriptLanguageValidationError AnyScriptLanguage AnyCardanoEra
  deriving Show

instance Error ScriptLanguageValidationError where
  prettyError = \case
    ScriptLanguageValidationError lang era ->
      "The script language " <> pshow lang <> " is not supported in the " <>
      pretty era <> " era."

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
    "Implicit transaction fee not supported in " <> pretty era
  prettyError (TxFeatureExplicitFeesE era) =
    "Explicit transaction fee not supported in " <> pretty era

validateTxFee :: ShelleyBasedEra era
              -> Maybe L.Coin -- TODO: Make this mandatory in the cli (Remove Maybe)
              -> Either TxFeeValidationError (TxFee era)
validateTxFee era = \case
  Nothing ->
    let cEra = toCardanoEra era
    in Left . TxFeatureImplicitFeesE $ cardanoEraConstraints cEra $ AnyCardanoEra cEra
  Just fee -> pure (TxFeeExplicit era fee)

newtype TxTotalCollateralValidationError
  = TxTotalCollateralNotSupported AnyCardanoEra
  deriving Show

instance Error TxTotalCollateralValidationError where
  prettyError (TxTotalCollateralNotSupported era) =
    "Transaction collateral not supported in " <> pretty era

validateTxTotalCollateral :: CardanoEra era
                          -> Maybe L.Coin
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
    "Transaction return collateral not supported in " <> pretty era

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
    "Transaction validity lower bound not supported in " <> pretty era


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
    "Transaction validity upper bound must be specified in " <> pretty era


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

newtype TxRequiredSignersValidationError
  = TxRequiredSignersValidationError AnyCardanoEra
  deriving Show

instance Error TxRequiredSignersValidationError where
  prettyError (TxRequiredSignersValidationError e) =
    "Transaction required signers are not supported in " <> pretty e

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
    "Transaction withdrawals are not supported in " <> pretty e

validateTxWithdrawals
  :: forall era.
     CardanoEra era
  -> [(StakeAddress, L.Coin, Maybe (ScriptWitness WitCtxStake era))]
  -> Either TxWithdrawalsValidationError (TxWithdrawals BuildTx era)
validateTxWithdrawals _ [] = return TxWithdrawalsNone
validateTxWithdrawals era withdrawals = do
  supported <- conjureWitness era TxWithdrawalsNotSupported
  let convWithdrawals = map convert withdrawals
  pure $ TxWithdrawals supported convWithdrawals
 where
  convert
    :: (StakeAddress, L.Coin, Maybe (ScriptWitness WitCtxStake era))
    -> (StakeAddress, L.Coin, BuildTxWith BuildTx (Witness WitCtxStake era))
  convert (sAddr, ll, mScriptWitnessFiles) =
    case mScriptWitnessFiles of
      Just sWit -> (sAddr, ll, BuildTxWith $ ScriptWitness ScriptWitnessForStakeAddr sWit)
      Nothing   -> (sAddr, ll, BuildTxWith $ KeyWitness KeyWitnessForStakeAddr)

newtype TxCertificatesValidationError
  = TxCertificatesValidationNotSupported AnyCardanoEra
  deriving Show

instance Error TxCertificatesValidationError where
  prettyError (TxCertificatesValidationNotSupported e) =
    "Transaction certificates are not supported in " <> pretty e

-- TODO: Because we have separated Byron related transaction
-- construction into separate commands, we can parameterize this
-- on ShelleyBasedEra era and remove Either TxCertificatesValidationError
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
    "Transaction protocol parameters are not supported in " <> pretty e

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
    "Transaction update proposal is not supported in " <> pretty e

newtype TxScriptValidityValidationError
  = ScriptValidityNotSupported AnyCardanoEra
  deriving Show

instance Error TxScriptValidityValidationError where
  prettyError (ScriptValidityNotSupported e) =
    "Transaction script validity is not supported in " <> pretty e

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

getVotingScriptCredentials
  :: VotingProcedures era
  -> Maybe (L.Voter (L.EraCrypto (ShelleyLedgerEra era)))
getVotingScriptCredentials (VotingProcedures (L.VotingProcedures m)) =
  listToMaybe $ Map.keys m

votingScriptWitnessSingleton
  :: VotingProcedures era
  -> Maybe (ScriptWitness WitCtxStake era)
  -> Map (L.Voter (L.EraCrypto (ShelleyLedgerEra era))) (ScriptWitness WitCtxStake era)
votingScriptWitnessSingleton _ Nothing = Map.empty
votingScriptWitnessSingleton votingProcedures (Just scriptWitness) =
  let voter = fromJust $ getVotingScriptCredentials votingProcedures
  in Map.singleton voter scriptWitness

newtype TxGovDuplicateVotes era =
  TxGovDuplicateVotes [L.GovActionId (L.EraCrypto (ShelleyLedgerEra era))]

instance Error (TxGovDuplicateVotes era) where
  prettyError (TxGovDuplicateVotes actionIds) =
    "Trying to merge votes with similar action identifiers: " <> viaShow actionIds <>
      ". This would cause ignoring some of the votes, so not proceeding."

-- TODO: We fold twice, we can do it in a single fold
convertToTxVotingProcedures
 :: [(VotingProcedures era, Maybe (ScriptWitness WitCtxStake era))]
 -> Either (TxGovDuplicateVotes era) (TxVotingProcedures BuildTx era)
convertToTxVotingProcedures votingProcedures = do
  VotingProcedures procedure <- foldM f emptyVotingProcedures votingProcedures
  pure $ TxVotingProcedures procedure (BuildTxWith votingScriptWitnessMap)
  where
    votingScriptWitnessMap = foldl (\acc next -> acc `Map.union` uncurry votingScriptWitnessSingleton next)
                               Map.empty
                               votingProcedures
    f acc (procedure, _witness) = mergeVotingProcedures acc procedure

mergeVotingProcedures :: ()
  => VotingProcedures era
  -> VotingProcedures era
  -> Either (TxGovDuplicateVotes era) (VotingProcedures era) -- ^ Either an error message, or the merged voting procedures
mergeVotingProcedures vpsa vpsb =
  VotingProcedures . L.VotingProcedures <$> foldM mergeVotesOfOneVoter Map.empty allVoters
  where
    mapa = L.unVotingProcedures (unVotingProcedures vpsa)
    mapb = L.unVotingProcedures (unVotingProcedures vpsb)
    allVoters = Set.union (Map.keysSet mapa) (Map.keysSet mapb)
    mergeVotesOfOneVoter acc voter =
      Map.union acc <$> case (Map.lookup voter mapa, Map.lookup voter mapb) of
        (Just v, Nothing) -> Right $ Map.singleton voter v -- Take only available value
        (Nothing, Just v) -> Right $ Map.singleton voter v -- Take only available value
        (Nothing, Nothing) -> Right Map.empty -- No value
        (Just va, Just vb) -> -- Here's the case where we're unioning different votes for the same voter
          if null intersection -- No conflict: sets of keys from left and right is disjoint
          then Right $ Map.singleton voter (Map.union va vb)
          else Left $ TxGovDuplicateVotes intersection -- Ooops conflict! Let's report it!
          where
            intersection = Map.keys $ Map.intersection va vb

proposingScriptWitnessSingleton
  :: Proposal era
  -> Maybe (ScriptWitness WitCtxStake era)
  -> Map (L.ProposalProcedure (ShelleyLedgerEra era)) (ScriptWitness WitCtxStake era)
proposingScriptWitnessSingleton _ Nothing = Map.empty
proposingScriptWitnessSingleton (Proposal proposalProcedure) (Just scriptWitness) =
  Map.singleton proposalProcedure scriptWitness

convToTxProposalProcedures
  :: L.EraPParams (ShelleyLedgerEra era)
  => [(Proposal era, Maybe (ScriptWitness WitCtxStake era))]
  -> TxProposalProcedures BuildTx era
convToTxProposalProcedures proposalProcedures =
   -- TODO: Ledger does not export snoc so we can't fold here.
   let proposals = OSet.fromFoldable $ map (unProposal . fst) proposalProcedures
       sWitMap = BuildTxWith $ foldl sWitMapFolder Map.empty proposalProcedures
   in TxProposalProcedures proposals sWitMap
  where
   sWitMapFolder sWitMapAccum nextSWit = sWitMapAccum `Map.union` uncurry proposingScriptWitnessSingleton nextSWit

