{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Unused LANGUAGE pragma" -}

module Cardano.CLI.Types.Transaction
  ( runTxBuildRaw
  , runTxBuild
  , txFeatureMismatch
  , txFeatureMismatchPure
  , validateTxIns
  , validateTxInsCollateral
  , validateTxInsReference
  , getAllReferenceInputs
  , toAddressInAnyEra
  , toTxOutValueInAnyEra
  , toTxOutInAnyEra
  , readValueScriptWitnesses
  , partitionSomeWitnesses
  , mkShelleyBootstrapWitness
  , mkShelleyBootstrapWitnesses
  , onlyInShelleyBasedEras
  ) where

import           Cardano.Api
import           Cardano.Api.Byron hiding (SomeByronSigningKey (..))
import           Cardano.Api.Shelley

import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.ShelleyBootstrapWitnessError
import           Cardano.CLI.Types.Errors.ShelleyTxCmdError
import           Cardano.CLI.Types.Errors.TxValidationError
import           Cardano.CLI.Types.TxFeature

import           Control.Monad (forM)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans (MonadTrans (..))
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, left, onLeft,
                   onNothing)
import           Data.Bifunctor (Bifunctor (..))
import           Data.Foldable (Foldable (..))
import           Data.Function ((&))
import           Data.Functor
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)

-- ----------------------------------------------------------------------------
-- Building transactions
--

runTxBuildRaw :: ()
  => CardanoEra era
  -> Maybe ScriptValidity
  -- ^ Mark script as expected to pass or fail validation
  -> [(TxIn, Maybe (ScriptWitness WitCtxTxIn era))]
  -- ^ TxIn with potential script witness
  -> [TxIn]
  -- ^ Read only reference inputs
  -> [TxIn]
  -- ^ TxIn for collateral
  -> Maybe (TxOut CtxTx era)
  -- ^ Return collateral
  -> Maybe Lovelace
  -- ^ Total collateral
  -> [TxOut CtxTx era]
  -> Maybe SlotNo
  -- ^ Tx lower bound
  -> Maybe SlotNo
  -- ^ Tx upper bound
  -> Maybe Lovelace
  -- ^ Tx fee
  -> (Value, [ScriptWitness WitCtxMint era])
  -- ^ Multi-Asset value(s)
  -> [(Certificate era, Maybe (ScriptWitness WitCtxStake era))]
  -- ^ Certificate with potential script witness
  -> [(StakeAddress, Lovelace, Maybe (ScriptWitness WitCtxStake era))]
  -> [Hash PaymentKey]
  -- ^ Required signers
  -> TxAuxScripts era
  -> TxMetadataInEra era
  -> Maybe ProtocolParameters
  -> Maybe UpdateProposal
  -> Either ShelleyTxCmdError (TxBody era)
runTxBuildRaw era
              mScriptValidity inputsAndMaybeScriptWits
              readOnlyRefIns txinsc
              mReturnCollateral mTotCollateral txouts
              mLowerBound mUpperBound
              mFee valuesWithScriptWits
              certsAndMaybeSriptWits withdrawals reqSigners
              txAuxScripts txMetadata mpparams mUpdateProp = do

    let allReferenceInputs = getAllReferenceInputs
                               inputsAndMaybeScriptWits
                               (snd valuesWithScriptWits)
                               certsAndMaybeSriptWits
                               withdrawals
                               readOnlyRefIns

    validatedCollateralTxIns <- validateTxInsCollateral era txinsc
    validatedRefInputs <- validateTxInsReference era allReferenceInputs
    validatedTotCollateral
      <- first ShelleyTxCmdTotalCollateralValidationError $ validateTxTotalCollateral era mTotCollateral
    validatedRetCol
      <- first ShelleyTxCmdReturnCollateralValidationError $ validateTxReturnCollateral era mReturnCollateral
    validatedFee
      <- first ShelleyTxCmdTxFeeValidationError $ validateTxFee era mFee
    validatedBounds <- (,) <$> first ShelleyTxCmdTxValidityLowerBoundValidationError (validateTxValidityLowerBound era mLowerBound)
                           <*> first ShelleyTxCmdTxValidityUpperBoundValidationError (validateTxValidityUpperBound era mUpperBound)
    validatedReqSigners
      <- first ShelleyTxCmdRequiredSignersValidationError $ validateRequiredSigners era reqSigners
    validatedPParams
      <- first ShelleyTxCmdProtocolParametersValidationError $ validateProtocolParameters era mpparams
    validatedTxWtdrwls
      <- first ShelleyTxCmdTxWithdrawalsValidationError $ validateTxWithdrawals era withdrawals
    validatedTxCerts
      <- first ShelleyTxCmdTxCertificatesValidationError $ validateTxCertificates era certsAndMaybeSriptWits
    validatedTxUpProp
      <- first ShelleyTxCmdTxUpdateProposalValidationError $ validateTxUpdateProposal era mUpdateProp
    validatedMintValue
      <- createTxMintValue era valuesWithScriptWits
    validatedTxScriptValidity
      <- first ShelleyTxCmdScriptValidityValidationError $ validateTxScriptValidity era mScriptValidity
    let validatedTxGovernanceActions = TxGovernanceActionsNone -- TODO: Conwary era
        validatedTxVotes = Nothing -- TODO: Conwary era
    let txBodyContent =
          TxBodyContent
            { txIns = validateTxIns inputsAndMaybeScriptWits
            , txInsCollateral = validatedCollateralTxIns
            , txInsReference = validatedRefInputs
            , txOuts = txouts
            , txTotalCollateral = validatedTotCollateral
            , txReturnCollateral = validatedRetCol
            , txFee = validatedFee
            , txValidityRange = validatedBounds
            , txMetadata = txMetadata
            , txAuxScripts = txAuxScripts
            , txExtraKeyWits = validatedReqSigners
            , txProtocolParams = validatedPParams
            , txWithdrawals = validatedTxWtdrwls
            , txCertificates = validatedTxCerts
            , txUpdateProposal = validatedTxUpProp
            , txMintValue = validatedMintValue
            , txScriptValidity = validatedTxScriptValidity
            , txGovernanceActions = validatedTxGovernanceActions
            , txVotingProcedures = validatedTxVotes
            }

    first ShelleyTxCmdTxBodyError $
      getIsCardanoEraConstraint era $ createAndValidateTransactionBody txBodyContent

runTxBuild :: ()
  => CardanoEra era
  -> SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe ScriptValidity
  -- ^ Mark script as expected to pass or fail validation
  -> [(TxIn, Maybe (ScriptWitness WitCtxTxIn era))]
  -- ^ Read only reference inputs
  -> [TxIn]
  -- ^ TxIn with potential script witness
  -> [TxIn]
  -- ^ TxIn for collateral
  -> Maybe (TxOut CtxTx era)
  -- ^ Return collateral
  -> Maybe Lovelace
  -- ^ Total collateral
  -> [TxOut CtxTx era]
  -- ^ Normal outputs
  -> TxOutChangeAddress
  -- ^ A change output
  -> (Value, [ScriptWitness WitCtxMint era])
  -- ^ Multi-Asset value(s)
  -> Maybe SlotNo
  -- ^ Tx lower bound
  -> Maybe SlotNo
  -- ^ Tx upper bound
  -> [(Certificate era, Maybe (ScriptWitness WitCtxStake era))]
  -- ^ Certificate with potential script witness
  -> [(StakeAddress, Lovelace, Maybe (ScriptWitness WitCtxStake era))]
  -> [Hash PaymentKey]
  -- ^ Required signers
  -> TxAuxScripts era
  -> TxMetadataInEra era
  -> Maybe UpdateProposal
  -> Maybe Word
  -> VotingProcedures era
  -> TxGovernanceActions era
  -> TxBuildOutputOptions
  -> ExceptT ShelleyTxCmdError IO (BalancedTxBody era)
runTxBuild
    era socketPath (AnyConsensusModeParams cModeParams) networkId mScriptValidity
    inputsAndMaybeScriptWits readOnlyRefIns txinsc mReturnCollateral mTotCollateral txouts
    (TxOutChangeAddress changeAddr) valuesWithScriptWits mLowerBound mUpperBound
    certsAndMaybeScriptWits withdrawals reqSigners txAuxScripts txMetadata
    mUpdatePropF mOverrideWits votes proposals outputOptions = do

  let consensusMode = consensusModeOnly cModeParams
      dummyFee = Just $ Lovelace 0
      inputsThatRequireWitnessing = [input | (input,_) <- inputsAndMaybeScriptWits]

  -- Pure
  let allReferenceInputs = getAllReferenceInputs
                             inputsAndMaybeScriptWits
                             (snd valuesWithScriptWits)
                             certsAndMaybeScriptWits
                             withdrawals readOnlyRefIns

  validatedCollateralTxIns <- hoistEither $ validateTxInsCollateral era txinsc
  validatedRefInputs <- hoistEither $ validateTxInsReference era allReferenceInputs
  validatedTotCollateral
    <- hoistEither $ first ShelleyTxCmdTotalCollateralValidationError $ validateTxTotalCollateral era mTotCollateral
  validatedRetCol
    <- hoistEither $ first ShelleyTxCmdReturnCollateralValidationError $ validateTxReturnCollateral era mReturnCollateral
  dFee <- hoistEither $ first ShelleyTxCmdTxFeeValidationError $ validateTxFee era dummyFee
  validatedBounds <- (,) <$> hoistEither (first ShelleyTxCmdTxValidityLowerBoundValidationError $ validateTxValidityLowerBound era mLowerBound)
                         <*> hoistEither (first ShelleyTxCmdTxValidityUpperBoundValidationError $ validateTxValidityUpperBound era mUpperBound)
  validatedReqSigners <- hoistEither (first ShelleyTxCmdRequiredSignersValidationError $ validateRequiredSigners era reqSigners)
  validatedTxWtdrwls <- hoistEither (first ShelleyTxCmdTxWithdrawalsValidationError $ validateTxWithdrawals era withdrawals)
  validatedTxCerts <- hoistEither (first ShelleyTxCmdTxCertificatesValidationError $ validateTxCertificates era certsAndMaybeScriptWits)
  validatedTxUpProp <- hoistEither (first ShelleyTxCmdTxUpdateProposalValidationError $ validateTxUpdateProposal era mUpdatePropF)
  validatedMintValue <- hoistEither $ createTxMintValue era valuesWithScriptWits
  validatedTxScriptValidity <- hoistEither (first ShelleyTxCmdScriptValidityValidationError $ validateTxScriptValidity era mScriptValidity)

  case (consensusMode, cardanoEraStyle era) of
    (CardanoMode, ShelleyBasedEra sbe) -> do
      void $ pure (toEraInMode era CardanoMode)
        & onNothing (left (ShelleyTxCmdEraConsensusModeMismatchTxBalance outputOptions
                            (AnyConsensusMode CardanoMode) (AnyCardanoEra era)))

      let allTxInputs = inputsThatRequireWitnessing ++ allReferenceInputs ++ txinsc
          localNodeConnInfo = LocalNodeConnectInfo
                                     { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
                                     , localNodeNetworkId = networkId
                                     , localNodeSocketPath = socketPath
                                     }

      AnyCardanoEra nodeEra <- lift (executeLocalStateQueryExpr localNodeConnInfo Nothing (determineEraExpr cModeParams))
        & onLeft (left . ShelleyTxCmdQueryConvenienceError . AcqFailure)
        & onLeft (left . ShelleyTxCmdQueryConvenienceError . QceUnsupportedNtcVersion)

      let certs =
            case validatedTxCerts of
              TxCertificates _ cs _ -> cs
              _ -> []

      nodeEraCerts <- pure (forM certs $ eraCast nodeEra)
        & onLeft (left . ShelleyTxCmdTxEraCastErr)

      (nodeEraUTxO, pparams, eraHistory, systemStart, stakePools, stakeDelegDeposits) <-
        lift (executeLocalStateQueryExpr localNodeConnInfo Nothing $ queryStateForBalancedTx nodeEra allTxInputs nodeEraCerts)
          & onLeft (left . ShelleyTxCmdQueryConvenienceError . AcqFailure)
          & onLeft (left . ShelleyTxCmdQueryConvenienceError)

      pp <- hoistEither . first ShelleyTxCmdProtocolParamsConverstionError $ toLedgerPParams sbe pparams

      validatedPParams <- hoistEither $ first ShelleyTxCmdProtocolParametersValidationError
                                      $ validateProtocolParameters era (Just pparams)

      let validatedTxGovernanceActions = proposals
          validatedTxVotes =  votes
          txBodyContent =
            TxBodyContent
              { txIns = validateTxIns inputsAndMaybeScriptWits
              , txInsCollateral = validatedCollateralTxIns
              , txInsReference = validatedRefInputs
              , txOuts = txouts
              , txTotalCollateral = validatedTotCollateral
              , txReturnCollateral = validatedRetCol
              , txFee = dFee
              , txValidityRange = validatedBounds
              , txMetadata = txMetadata
              , txAuxScripts = txAuxScripts
              , txExtraKeyWits = validatedReqSigners
              , txProtocolParams = validatedPParams
              , txWithdrawals = validatedTxWtdrwls
              , txCertificates = validatedTxCerts
              , txUpdateProposal = validatedTxUpProp
              , txMintValue = validatedMintValue
              , txScriptValidity = validatedTxScriptValidity
              , txGovernanceActions = validatedTxGovernanceActions
              , txVotingProcedures = inEraFeature era Nothing (\w -> Just (Featured w validatedTxVotes))
              }

      firstExceptT ShelleyTxCmdTxInsDoNotExist
        . hoistEither $ txInsExistInUTxO allTxInputs nodeEraUTxO
      firstExceptT ShelleyTxCmdQueryNotScriptLocked
        . hoistEither $ notScriptLockedTxIns txinsc nodeEraUTxO

      cAddr <- pure (anyAddressInEra era changeAddr)
        & onLeft (error $ "runTxBuild: Byron address used: " <> show changeAddr) -- should this throw instead?

      -- Why do we cast the era? The user can specify an era prior to the era that the node is currently in.
      -- We cannot use the user specified era to construct a query against a node because it may differ
      -- from the node's era and this will result in the 'QueryEraMismatch' failure.
      txEraUtxo <- pure (eraCast era nodeEraUTxO) & onLeft (left . ShelleyTxCmdTxEraCastErr)

      balancedTxBody@(BalancedTxBody _ _ _ fee) <-
        firstExceptT ShelleyTxCmdBalanceTxBody
          . hoistEither
          $ makeTransactionBodyAutoBalance systemStart (toLedgerEpochInfo eraHistory)
                                           pp stakePools stakeDelegDeposits txEraUtxo
                                           txBodyContent cAddr mOverrideWits

      liftIO $ putStrLn $ "Estimated transaction fee: " <> (show fee :: String)

      return balancedTxBody

    (CardanoMode, LegacyByronEra) -> left ShelleyTxCmdByronEra

    (wrongMode, _) -> left (ShelleyTxCmdUnsupportedMode (AnyConsensusMode wrongMode))

-- ----------------------------------------------------------------------------
-- Transaction body validation and conversion
--

txFeatureMismatch :: ()
  => Monad m
  => CardanoEra era
  -> TxFeature
  -> ExceptT ShelleyTxCmdError m a
txFeatureMismatch era feature =
    hoistEither . Left $ ShelleyTxCmdTxFeatureMismatch (anyCardanoEra era) feature

txFeatureMismatchPure :: ()
  => CardanoEra era
  -> TxFeature
  -> Either ShelleyTxCmdError a
txFeatureMismatchPure era feature =
    Left (ShelleyTxCmdTxFeatureMismatch (anyCardanoEra era) feature)

validateTxIns :: ()
  => [(TxIn, Maybe (ScriptWitness WitCtxTxIn era))]
  -> [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
validateTxIns = map convert
 where
   convert
     :: (TxIn, Maybe (ScriptWitness WitCtxTxIn era))
     -> (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))
   convert (txin, mScriptWitness) =
     case mScriptWitness of
       Just sWit ->
         (txin , BuildTxWith $ ScriptWitness ScriptWitnessForSpending sWit)
       Nothing ->
         (txin, BuildTxWith $ KeyWitness KeyWitnessForSpending)


validateTxInsCollateral :: ()
  => CardanoEra era
  -> [TxIn]
  -> Either ShelleyTxCmdError (TxInsCollateral era)
validateTxInsCollateral _   []    = return TxInsCollateralNone
validateTxInsCollateral era txins =
    case collateralSupportedInEra era of
      Nothing -> txFeatureMismatchPure era TxFeatureCollateral
      Just supported -> return (TxInsCollateral supported txins)

validateTxInsReference :: ()
  => CardanoEra era
  -> [TxIn]
  -> Either ShelleyTxCmdError (TxInsReference BuildTx era)
validateTxInsReference _ []  = return TxInsReferenceNone
validateTxInsReference era allRefIns =
  case refInsScriptsAndInlineDatsSupportedInEra era of
    Nothing -> txFeatureMismatchPure era TxFeatureReferenceInputs
    Just supp -> return $ TxInsReference supp allRefIns


getAllReferenceInputs :: ()
  => [(TxIn, Maybe (ScriptWitness WitCtxTxIn era))]
  -> [ScriptWitness WitCtxMint era]
  -> [(Certificate era, Maybe (ScriptWitness WitCtxStake era))]
  -> [(StakeAddress, Lovelace, Maybe (ScriptWitness WitCtxStake era))]
  -> [TxIn] -- ^ Read only reference inputs
  -> [TxIn]
getAllReferenceInputs txins mintWitnesses certFiles withdrawals readOnlyRefIns = do
  let txinsWitByRefInputs = [getReferenceInput sWit | (_, Just sWit) <- txins]
      mintingRefInputs = map getReferenceInput mintWitnesses
      certsWitByRefInputs = [getReferenceInput sWit | (_, Just sWit) <- certFiles]
      withdrawalsWitByRefInputs = [getReferenceInput sWit | (_, _, Just sWit) <- withdrawals]

  catMaybes $ concat [ txinsWitByRefInputs
                     , mintingRefInputs
                     , certsWitByRefInputs
                     , withdrawalsWitByRefInputs
                     , map Just readOnlyRefIns
                     ]
  where
    getReferenceInput
      :: ScriptWitness witctx era -> Maybe TxIn
    getReferenceInput sWit =
      case sWit of
        PlutusScriptWitness _ _ (PReferenceScript refIn _) _ _ _ -> Just refIn
        PlutusScriptWitness _ _ PScript{} _ _ _ -> Nothing
        SimpleScriptWitness _ (SReferenceScript refIn _)  -> Just refIn
        SimpleScriptWitness _ SScript{}  -> Nothing

toAddressInAnyEra :: ()
  => CardanoEra era
  -> AddressAny
  -> Either ShelleyTxCmdError (AddressInEra era)
toAddressInAnyEra era addrAny = runExcept $ do
  case addrAny of
    AddressByron   bAddr -> pure (AddressInEra ByronAddressInAnyEra bAddr)
    AddressShelley sAddr -> do
      sbe <- requireShelleyBasedEra era
        & onNothing (txFeatureMismatch era TxFeatureShelleyAddresses)

      pure (AddressInEra (ShelleyAddressInEra sbe) sAddr)

toTxOutValueInAnyEra :: ()
  => CardanoEra era
  -> Value
  -> Either ShelleyTxCmdError (TxOutValue era)
toTxOutValueInAnyEra era val =
  case multiAssetSupportedInEra era of
    Left adaOnlyInEra ->
      case valueToLovelace val of
        Just l  -> return (TxOutAdaOnly adaOnlyInEra l)
        Nothing -> txFeatureMismatchPure era TxFeatureMultiAssetOutputs
    Right multiAssetInEra -> return (TxOutValue multiAssetInEra val)

toTxOutInAnyEra :: ()
  => CardanoEra era
  -> TxOutAnyEra
  -> ExceptT ShelleyTxCmdError IO (TxOut CtxTx era)
toTxOutInAnyEra era (TxOutAnyEra addr' val' mDatumHash refScriptFp) = do
  addr <- hoistEither $ toAddressInAnyEra era addr'
  val <- hoistEither $ toTxOutValueInAnyEra era val'
  (datum, refScript)
    <- case (scriptDataSupportedInEra era, refInsScriptsAndInlineDatsSupportedInEra era) of
         (Nothing, Nothing) -> pure (TxOutDatumNone, ReferenceScriptNone)
         (Just sup, Nothing)->
           (,) <$> toTxAlonzoDatum sup mDatumHash <*> pure ReferenceScriptNone
         (Just sup, Just inlineDatumRefScriptSupp) ->
           toTxDatumReferenceScriptBabbage sup inlineDatumRefScriptSupp mDatumHash refScriptFp
         (Nothing, Just _) ->
           -- TODO: Figure out how to make this state unrepresentable
           error "toTxOutInAnyEra: Should not be possible that inline datums are allowed but datums are not"
  pure $ TxOut addr val datum refScript
  where
    getReferenceScript
      :: ReferenceScriptAnyEra
      -> ReferenceTxInsScriptsInlineDatumsSupportedInEra era
      -> ExceptT ShelleyTxCmdError IO (ReferenceScript era)
    getReferenceScript ReferenceScriptAnyEraNone _ = return ReferenceScriptNone
    getReferenceScript (ReferenceScriptAnyEra fp) supp = do
      ReferenceScript supp
        <$> firstExceptT ShelleyTxCmdScriptFileError (readFileScriptInAnyLang fp)

    toTxDatumReferenceScriptBabbage
      :: ScriptDataSupportedInEra era
      -> ReferenceTxInsScriptsInlineDatumsSupportedInEra era
      -> TxOutDatumAnyEra
      -> ReferenceScriptAnyEra
      -> ExceptT ShelleyTxCmdError IO (TxOutDatum CtxTx era, ReferenceScript era)
    toTxDatumReferenceScriptBabbage sDataSupp inlineRefSupp cliDatum refScriptFp' = do
      refScript <- getReferenceScript refScriptFp' inlineRefSupp
      case cliDatum of
        TxOutDatumByNone -> do
          pure (TxOutDatumNone, refScript)
        TxOutDatumByHashOnly dh -> do
          pure (TxOutDatumHash sDataSupp dh, refScript)
        TxOutDatumByHashOf fileOrSdata -> do
          sData <- firstExceptT ShelleyTxCmdScriptDataError
                      $ readScriptDataOrFile fileOrSdata
          pure (TxOutDatumHash sDataSupp $ hashScriptDataBytes sData, refScript)
        TxOutDatumByValue fileOrSdata -> do
          sData <- firstExceptT ShelleyTxCmdScriptDataError
                      $ readScriptDataOrFile fileOrSdata
          pure (TxOutDatumInTx sDataSupp sData, refScript)
        TxOutInlineDatumByValue fileOrSdata -> do
          sData <- firstExceptT ShelleyTxCmdScriptDataError
                      $ readScriptDataOrFile fileOrSdata
          pure (TxOutDatumInline inlineRefSupp sData, refScript)

    toTxAlonzoDatum
      :: ScriptDataSupportedInEra era
      -> TxOutDatumAnyEra
      -> ExceptT ShelleyTxCmdError IO (TxOutDatum CtxTx era)
    toTxAlonzoDatum supp cliDatum =
      case cliDatum of
        TxOutDatumByHashOnly h -> pure (TxOutDatumHash supp h)
        TxOutDatumByHashOf sDataOrFile -> do
          sData <- firstExceptT ShelleyTxCmdScriptDataError
                    $ readScriptDataOrFile sDataOrFile
          pure (TxOutDatumHash supp $ hashScriptDataBytes sData)
        TxOutDatumByValue sDataOrFile -> do
          sData <- firstExceptT ShelleyTxCmdScriptDataError
                    $ readScriptDataOrFile sDataOrFile
          pure (TxOutDatumInTx supp sData)
        TxOutInlineDatumByValue _ ->
          txFeatureMismatch era TxFeatureInlineDatums
        TxOutDatumByNone -> pure TxOutDatumNone

-- TODO: Currently we specify the policyId with the '--mint' option on the cli
-- and we added a separate '--policy-id' parser that parses the policy id for the
-- given reference input (since we don't have the script in this case). To avoid asking
-- for the policy id twice (in the build command) we can potentially query the UTxO and
-- access the script (and therefore the policy id).
createTxMintValue :: forall era. ()
  => CardanoEra era
  -> (Value, [ScriptWitness WitCtxMint era])
  -> Either ShelleyTxCmdError (TxMintValue BuildTx era)
createTxMintValue era (val, scriptWitnesses) =
  if List.null (valueToList val) && List.null scriptWitnesses
    then return TxMintNone
    else do
      case multiAssetSupportedInEra era of
        Left _ -> txFeatureMismatchPure era TxFeatureMintValue
        Right supported -> do
          -- The set of policy ids for which we need witnesses:
          let witnessesNeededSet :: Set PolicyId
              witnessesNeededSet =
                Set.fromList [ pid | (AssetId pid _, _) <- valueToList val ]

          let witnessesProvidedMap :: Map PolicyId (ScriptWitness WitCtxMint era)
              witnessesProvidedMap = Map.fromList $ gatherMintingWitnesses scriptWitnesses

              witnessesProvidedSet = Map.keysSet witnessesProvidedMap

          -- Check not too many, nor too few:
          validateAllWitnessesProvided   witnessesNeededSet witnessesProvidedSet
          validateNoUnnecessaryWitnesses witnessesNeededSet witnessesProvidedSet

          return (TxMintValue supported val (BuildTxWith witnessesProvidedMap))
  where
    gatherMintingWitnesses
      :: [ScriptWitness WitCtxMint era]
      -> [(PolicyId, ScriptWitness WitCtxMint era)]
    gatherMintingWitnesses [] = []
    gatherMintingWitnesses (sWit : rest) =
      case scriptWitnessPolicyId sWit of
        Nothing -> gatherMintingWitnesses rest
        Just pid -> (pid, sWit) : gatherMintingWitnesses rest

    validateAllWitnessesProvided witnessesNeeded witnessesProvided
      | null witnessesMissing = return ()
      | otherwise = Left (ShelleyTxCmdPolicyIdsMissing witnessesMissing)
      where
        witnessesMissing = Set.elems (witnessesNeeded Set.\\ witnessesProvided)

    validateNoUnnecessaryWitnesses witnessesNeeded witnessesProvided
      | null witnessesExtra = return ()
      | otherwise = Left (ShelleyTxCmdPolicyIdsExcess witnessesExtra)
      where
        witnessesExtra = Set.elems (witnessesProvided Set.\\ witnessesNeeded)

scriptWitnessPolicyId :: ScriptWitness witctx era -> Maybe PolicyId
scriptWitnessPolicyId (SimpleScriptWitness _ (SScript script)) =
   Just . scriptPolicyId $ SimpleScript script
scriptWitnessPolicyId (SimpleScriptWitness _ (SReferenceScript _ mPid)) =
   PolicyId <$> mPid
scriptWitnessPolicyId (PlutusScriptWitness _ version (PScript script) _ _ _) =
   Just . scriptPolicyId $ PlutusScript version script
scriptWitnessPolicyId (PlutusScriptWitness _ _ (PReferenceScript _ mPid) _ _ _) =
   PolicyId <$> mPid

readValueScriptWitnesses :: ()
  => CardanoEra era
  -> (Value, [ScriptWitnessFiles WitCtxMint])
  -> ExceptT ShelleyTxCmdError IO (Value, [ScriptWitness WitCtxMint era])
readValueScriptWitnesses era (v, sWitFiles) = do
  sWits <- mapM (firstExceptT ShelleyTxCmdScriptWitnessError . readScriptWitness era) sWitFiles
  return (v, sWits)

partitionSomeWitnesses :: ()
  => [ByronOrShelleyWitness]
  -> ([ShelleyBootstrapWitnessSigningKeyData], [ShelleyWitnessSigningKey])
partitionSomeWitnesses = reversePartitionedWits . foldl' go mempty
  where
    reversePartitionedWits (bw, skw) =
      (reverse bw, reverse skw)

    go (byronAcc, shelleyKeyAcc) byronOrShelleyWit =
      case byronOrShelleyWit of
        AByronWitness byronWit ->
          (byronWit:byronAcc, shelleyKeyAcc)
        AShelleyKeyWitness shelleyKeyWit ->
          (byronAcc, shelleyKeyWit:shelleyKeyAcc)

-- | Construct a Shelley bootstrap witness (i.e. a Byron key witness in the
-- Shelley era).
mkShelleyBootstrapWitness :: ()
  =>  IsShelleyBasedEra era
  => Maybe NetworkId
  -> TxBody era
  -> ShelleyBootstrapWitnessSigningKeyData
  -> Either ShelleyBootstrapWitnessError (KeyWitness era)
mkShelleyBootstrapWitness Nothing _ (ShelleyBootstrapWitnessSigningKeyData _ Nothing) =
  Left MissingNetworkIdOrByronAddressError
mkShelleyBootstrapWitness (Just nw) txBody (ShelleyBootstrapWitnessSigningKeyData skey Nothing) =
  Right $ makeShelleyBootstrapWitness (WitnessNetworkId nw) txBody skey
mkShelleyBootstrapWitness _ txBody (ShelleyBootstrapWitnessSigningKeyData skey (Just addr)) =
  Right $ makeShelleyBootstrapWitness (WitnessByronAddress addr) txBody skey

-- | Attempt to construct Shelley bootstrap witnesses until an error is
-- encountered.
mkShelleyBootstrapWitnesses :: ()
  => IsShelleyBasedEra era
  => Maybe NetworkId
  -> TxBody era
  -> [ShelleyBootstrapWitnessSigningKeyData]
  -> Either ShelleyBootstrapWitnessError [KeyWitness era]
mkShelleyBootstrapWitnesses mnw txBody =
  mapM (mkShelleyBootstrapWitness mnw txBody)

-- | Constrain the era to be Shelley based. Fail for the Byron era.
onlyInShelleyBasedEras :: ()
  => Text
  -> InAnyCardanoEra a
  -> ExceptT ShelleyTxCmdError IO (InAnyShelleyBasedEra a)
onlyInShelleyBasedEras notImplMsg (InAnyCardanoEra era x) =
  case cardanoEraStyle era of
    LegacyByronEra      -> left (ShelleyTxCmdNotImplemented notImplMsg)
    ShelleyBasedEra sbe -> return (InAnyShelleyBasedEra sbe x)
