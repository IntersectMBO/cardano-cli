{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.CLI.Legacy.Run.Transaction
  ( runLegacyTransactionCmds
  , readFileTx
  , toTxOutInAnyEra
  ) where

import           Cardano.Api hiding (txAuxScripts, txMetadata, txOuts)
import qualified Cardano.Api.Byron as Api
import qualified Cardano.Api.Shelley as Api

import           Cardano.CLI.Json.Friendly (friendlyTxBS, friendlyTxBodyBS)
import           Cardano.CLI.Legacy.Commands.Transaction
import           Cardano.CLI.Legacy.Run.Genesis
import           Cardano.CLI.Legacy.Run.Validate
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.BootstrapWitnessError
import           Cardano.CLI.Types.Errors.TxCmdError
import           Cardano.CLI.Types.Governance
import qualified Cardano.CLI.Types.Output as Cli
import           Cardano.CLI.Types.TxFeature
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx

import           Control.Monad (forM)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans (MonadTrans (..))
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, hoistMaybe, left,
                   newExceptT, onLeft, onNothing)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Bifunctor (Bifunctor (..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Data ((:~:) (..))
import           Data.Foldable (Foldable (..))
import           Data.Function ((&))
import           Data.Functor
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Type.Equality (TestEquality (..))
import qualified System.IO as IO

{- HLINT ignore "Use let" -}

runLegacyTransactionCmds :: LegacyTransactionCmds -> ExceptT TxCmdError IO ()
runLegacyTransactionCmds = \case
  TxBuild mNodeSocketPath era consensusModeParams nid mScriptValidity mOverrideWits txins readOnlyRefIns
          reqSigners txinsc mReturnColl mTotCollateral txouts changeAddr mValue mLowBound
          mUpperBound certs wdrls metadataSchema scriptFiles metadataFiles mUpProp mconwayVote
          mNewConstitution outputOptions -> do
    runTxBuildCmd mNodeSocketPath era consensusModeParams nid mScriptValidity mOverrideWits txins readOnlyRefIns
          reqSigners txinsc mReturnColl mTotCollateral txouts changeAddr mValue mLowBound
          mUpperBound certs wdrls metadataSchema scriptFiles metadataFiles mUpProp mconwayVote
          mNewConstitution outputOptions
  TxBuildRaw era mScriptValidity txins readOnlyRefIns txinsc mReturnColl
              mTotColl reqSigners txouts mValue mLowBound mUpperBound fee certs wdrls
              metadataSchema scriptFiles metadataFiles mProtocolParamsFile mUpProp out -> do
    runTxBuildRawCmd era mScriptValidity txins readOnlyRefIns txinsc mReturnColl
              mTotColl reqSigners txouts mValue mLowBound mUpperBound fee certs wdrls
              metadataSchema scriptFiles metadataFiles mProtocolParamsFile mUpProp out
  TxSign txinfile skfiles network txoutfile ->
    runTxSign txinfile skfiles network txoutfile
  TxSubmit mNodeSocketPath anyConsensusModeParams network txFp ->
    runTxSubmit mNodeSocketPath anyConsensusModeParams network txFp
  TxCalculateMinFee txbody nw pParamsFile nInputs nOutputs nShelleyKeyWitnesses nByronKeyWitnesses ->
    runTxCalculateMinFee txbody nw pParamsFile nInputs nOutputs nShelleyKeyWitnesses nByronKeyWitnesses
  TxCalculateMinRequiredUTxO era pParamsFile txOuts -> runTxCalculateMinRequiredUTxO era pParamsFile txOuts
  TxHashScriptData scriptDataOrFile -> runTxHashScriptData scriptDataOrFile
  TxGetTxId txinfile -> runTxGetTxId txinfile
  TxView txinfile -> runTxView txinfile
  TxMintedPolicyId sFile -> runTxCreatePolicyId sFile
  TxCreateWitness txBodyfile witSignData mbNw outFile ->
    runTxCreateWitness txBodyfile witSignData mbNw outFile
  TxAssembleTxBodyWitness txBodyFile witnessFile outFile ->
    runTxSignWitness txBodyFile witnessFile outFile

-- ----------------------------------------------------------------------------
-- Building transactions
--

runTxBuildCmd
  :: SocketPath
  -> AnyCardanoEra
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe ScriptValidity
  -> Maybe Word -- ^ Override the required number of tx witnesses
  -> [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))] -- ^ Transaction inputs with optional spending scripts
  -> [TxIn] -- ^ Read only reference inputs
  -> [RequiredSigner] -- ^ Required signers
  -> [TxIn] -- ^ Transaction inputs for collateral, only key witnesses, no scripts.
  -> Maybe TxOutAnyEra -- ^ Return collateral
  -> Maybe Lovelace -- ^ Total collateral
  -> [TxOutAnyEra]
  -> TxOutChangeAddress
  -> Maybe (Value, [ScriptWitnessFiles WitCtxMint])
  -> Maybe SlotNo -- ^ Validity lower bound
  -> Maybe SlotNo -- ^ Validity upper bound
  -> [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
  -> [(StakeAddress, Lovelace, Maybe (ScriptWitnessFiles WitCtxStake))] -- ^ Withdrawals with potential script witness
  -> TxMetadataJsonSchema
  -> [ScriptFile]
  -> [MetadataFile]
  -> Maybe UpdateProposalFile
  -> [VoteFile In]
  -> [NewConstitutionFile In] -- TODO: Conway era - we should replace this with a sumtype that handles all governance actions
  -> TxBuildOutputOptions
  -> ExceptT TxCmdError IO ()
runTxBuildCmd
    socketPath (AnyCardanoEra cEra) consensusModeParams@(AnyConsensusModeParams cModeParams) nid
    mScriptValidity mOverrideWits txins readOnlyRefIns reqSigners txinsc mReturnColl mTotCollateral txouts
    changeAddr mValue mLowBound mUpperBound certs wdrls metadataSchema scriptFiles metadataFiles mUpProp
    conwayVotes newConstitutions outputOptions = do

  -- The user can specify an era prior to the era that the node is currently in.
  -- We cannot use the user specified era to construct a query against a node because it may differ
  -- from the node's era and this will result in the 'QueryEraMismatch' failure.

  let localNodeConnInfo = LocalNodeConnectInfo
                            { localConsensusModeParams = cModeParams
                            , localNodeNetworkId = nid
                            , localNodeSocketPath = socketPath
                            }

  inputsAndMaybeScriptWits <- firstExceptT TxCmdScriptWitnessError $ readScriptWitnessFiles cEra txins
  certFilesAndMaybeScriptWits <- firstExceptT TxCmdScriptWitnessError $ readScriptWitnessFiles cEra certs

  -- TODO: Conway Era - How can we make this more composable?
  certsAndMaybeScriptWits <-
    case cardanoEraStyle cEra of
      LegacyByronEra -> return []
      ShelleyBasedEra{} ->
        sequence
          [ fmap (,mSwit) (firstExceptT TxCmdReadTextViewFileError . newExceptT $
              readFileTextEnvelope AsCertificate (File certFile))
          | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
          ]
  withdrawalsAndMaybeScriptWits <- firstExceptT TxCmdScriptWitnessError
                                     $ readScriptWitnessFilesThruple cEra wdrls
  txMetadata <- firstExceptT TxCmdMetadataError
                  . newExceptT $ readTxMetadata cEra metadataSchema metadataFiles
  valuesWithScriptWits <- readValueScriptWitnesses cEra $ fromMaybe (mempty, []) mValue
  scripts <- firstExceptT TxCmdScriptFileError $
                     mapM (readFileScriptInAnyLang . unScriptFile) scriptFiles
  txAuxScripts <- hoistEither $ first TxCmdAuxScriptsValidationError $ validateTxAuxScripts cEra scripts

  mProp <- forM mUpProp $ \(UpdateProposalFile upFp) ->
    firstExceptT TxCmdReadTextViewFileError (newExceptT $ readFileTextEnvelope AsUpdateProposal (File upFp))
  requiredSigners  <- mapM (firstExceptT TxCmdRequiredSignerError .  newExceptT . readRequiredSigner) reqSigners
  mReturnCollateral <- forM mReturnColl $ toTxOutInAnyEra cEra

  txOuts <- mapM (toTxOutInAnyEra cEra) txouts

  -- Conway related
  votes <-
    featureInEra
      (pure TxVotesNone)
      (\w -> firstExceptT TxCmdVoteError $ ExceptT (readTxVotes w conwayVotes))
      cEra

  proposals <- newExceptT $ first TxCmdConstitutionError
                  <$> readTxNewConstitutionActions cEra newConstitutions


  -- the same collateral input can be used for several plutus scripts
  let filteredTxinsc = Set.toList $ Set.fromList txinsc

  -- We need to construct the txBodycontent outside of runTxBuild
  BalancedTxBody txBodycontent balancedTxBody _ _ <-
    runTxBuild
      socketPath cEra consensusModeParams nid mScriptValidity inputsAndMaybeScriptWits readOnlyRefIns filteredTxinsc
      mReturnCollateral mTotCollateral txOuts changeAddr valuesWithScriptWits mLowBound
      mUpperBound certsAndMaybeScriptWits withdrawalsAndMaybeScriptWits
      requiredSigners txAuxScripts txMetadata mProp mOverrideWits votes proposals outputOptions

  mScriptWits <-
    case cardanoEraStyle cEra of
      LegacyByronEra -> return []
      ShelleyBasedEra sbe -> return $ collectTxBodyScriptWitnesses sbe txBodycontent

  let allReferenceInputs = getAllReferenceInputs
                             inputsAndMaybeScriptWits
                             (snd valuesWithScriptWits)
                             certsAndMaybeScriptWits
                             withdrawalsAndMaybeScriptWits
                             readOnlyRefIns

  let inputsThatRequireWitnessing = [input | (input,_) <- inputsAndMaybeScriptWits]
      allTxInputs = inputsThatRequireWitnessing ++ allReferenceInputs ++ filteredTxinsc

  -- TODO: Calculating the script cost should live as a different command.
  -- Why? Because then we can simply read a txbody and figure out
  -- the script cost vs having to build the tx body each time
  case outputOptions of
    OutputScriptCostOnly fp -> do
      let BuildTxWith mTxProtocolParams = txProtocolParams txBodycontent

      pparams <- pure mTxProtocolParams & onNothing (left TxCmdProtocolParametersNotPresentInTxBodyError)

      executionUnitPrices <- pure (Api.protocolParamPrices pparams) & onNothing (left TxCmdPParamExecutionUnitsNotAvailableError)

      let consensusMode = consensusModeOnly cModeParams
      bpp <- hoistEither . first (TxCmdTxBodyError . TxBodyProtocolParamsConversionError) $
        bundleProtocolParams cEra pparams

      case consensusMode of
        CardanoMode -> do
          AnyCardanoEra nodeEra <- lift (executeLocalStateQueryExpr localNodeConnInfo Nothing (determineEraExpr cModeParams))
            & onLeft (left . TxCmdQueryConvenienceError . AcqFailure)
            & onLeft (left . TxCmdQueryConvenienceError . QceUnsupportedNtcVersion)

          (nodeEraUTxO, _, eraHistory, systemStart, _, _) <-
            lift (executeLocalStateQueryExpr localNodeConnInfo Nothing (queryStateForBalancedTx nodeEra allTxInputs []))
              & onLeft (left . TxCmdQueryConvenienceError . AcqFailure)
              & onLeft (left . TxCmdQueryConvenienceError)

          -- Why do we cast the era? The user can specify an era prior to the era that the node is currently in.
          -- We cannot use the user specified era to construct a query against a node because it may differ
          -- from the node's era and this will result in the 'QueryEraMismatch' failure.
          txEraUtxo <- pure (eraCast cEra nodeEraUTxO) & onLeft (left . TxCmdTxEraCastError)

          scriptExecUnitsMap <-
            firstExceptT TxCmdTxExecUnitsError $ hoistEither
              $ evaluateTransactionExecutionUnits
                  systemStart (toLedgerEpochInfo eraHistory)
                  bpp txEraUtxo balancedTxBody

          scriptCostOutput <-
            firstExceptT TxCmdPlutusScriptCostError $ hoistEither
              $ Cli.renderScriptCosts
                  txEraUtxo
                  executionUnitPrices
                  mScriptWits
                  scriptExecUnitsMap
          liftIO $ LBS.writeFile (unFile fp) $ encodePretty scriptCostOutput
        _ -> left TxCmdPlutusScriptsRequireCardanoModeError

    OutputTxBodyOnly fpath ->
      let noWitTx = makeSignedTransaction [] balancedTxBody
      in  lift (writeTxFileTextEnvelopeCddl fpath noWitTx)
            & onLeft (left . TxCmdWriteFileError)


runTxBuildRawCmd
  :: AnyCardanoEra
  -> Maybe ScriptValidity
  -> [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
  -> [TxIn] -- ^ Read only reference inputs
  -> [TxIn] -- ^ Transaction inputs for collateral, only key witnesses, no scripts.
  -> Maybe TxOutAnyEra
  -> Maybe Lovelace -- ^ Total collateral
  -> [RequiredSigner]
  -> [TxOutAnyEra]
  -> Maybe (Value, [ScriptWitnessFiles WitCtxMint]) -- ^ Multi-Asset value with script witness
  -> Maybe SlotNo -- ^ Validity lower bound
  -> Maybe SlotNo -- ^ Validity upper bound
  -> Maybe Lovelace -- ^ Tx fee
  -> [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
  -> [(StakeAddress, Lovelace, Maybe (ScriptWitnessFiles WitCtxStake))]
  -> TxMetadataJsonSchema
  -> [ScriptFile]
  -> [MetadataFile]
  -> Maybe ProtocolParamsFile
  -> Maybe UpdateProposalFile
  -> TxBodyFile Out
  -> ExceptT TxCmdError IO ()
runTxBuildRawCmd
  (AnyCardanoEra cEra) mScriptValidity txins readOnlyRefIns txinsc mReturnColl
  mTotColl reqSigners txouts mValue mLowBound mUpperBound fee certs wdrls
  metadataSchema scriptFiles metadataFiles mpParamsFile mUpProp out = do
  inputsAndMaybeScriptWits <- firstExceptT TxCmdScriptWitnessError
                                $ readScriptWitnessFiles cEra txins
  certFilesAndMaybeScriptWits <- firstExceptT TxCmdScriptWitnessError
                                   $ readScriptWitnessFiles cEra certs

  -- TODO: Conway era - How can we make this more composable?
  certsAndMaybeScriptWits <-
      case cardanoEraStyle cEra of
        LegacyByronEra -> return []
        ShelleyBasedEra{} ->
          sequence
            [ fmap (,mSwit) (firstExceptT TxCmdReadTextViewFileError . newExceptT $
                readFileTextEnvelope AsCertificate (File certFile))
            | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
            ]

  withdrawalsAndMaybeScriptWits <- firstExceptT TxCmdScriptWitnessError
                                     $ readScriptWitnessFilesThruple cEra wdrls
  txMetadata <- firstExceptT TxCmdMetadataError
                  . newExceptT $ readTxMetadata cEra metadataSchema metadataFiles
  valuesWithScriptWits <- readValueScriptWitnesses cEra $ fromMaybe (mempty, []) mValue
  scripts <- firstExceptT TxCmdScriptFileError $
                     mapM (readFileScriptInAnyLang . unScriptFile) scriptFiles
  txAuxScripts <- hoistEither $ first TxCmdAuxScriptsValidationError $ validateTxAuxScripts cEra scripts

  pparams <- forM mpParamsFile $ \ppf ->
    firstExceptT TxCmdProtocolParamsError (readProtocolParameters ppf)

  mProp <- forM mUpProp $ \(UpdateProposalFile upFp) ->
    firstExceptT TxCmdReadTextViewFileError (newExceptT $ readFileTextEnvelope AsUpdateProposal (File upFp))

  requiredSigners  <- mapM (firstExceptT TxCmdRequiredSignerError .  newExceptT . readRequiredSigner) reqSigners
  mReturnCollateral <- forM mReturnColl $ toTxOutInAnyEra cEra
  txOuts <- mapM (toTxOutInAnyEra cEra) txouts

    -- the same collateral input can be used for several plutus scripts
  let filteredTxinsc = Set.toList $ Set.fromList txinsc

  txBody <- hoistEither $ runTxBuildRaw cEra mScriptValidity inputsAndMaybeScriptWits readOnlyRefIns filteredTxinsc
                          mReturnCollateral mTotColl txOuts mLowBound mUpperBound fee valuesWithScriptWits
                          certsAndMaybeScriptWits withdrawalsAndMaybeScriptWits requiredSigners txAuxScripts
                          txMetadata pparams mProp

  let noWitTx = makeSignedTransaction [] txBody
  lift (getIsCardanoEraConstraint cEra $ writeTxFileTextEnvelopeCddl out noWitTx)
    & onLeft (left . TxCmdWriteFileError)


runTxBuildRaw
  :: CardanoEra era
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
  -> Maybe Api.ProtocolParameters
  -> Maybe UpdateProposal
  -> Either TxCmdError (TxBody era)
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
      <- first TxCmdTotalCollateralValidationError $ validateTxTotalCollateral era mTotCollateral
    validatedRetCol
      <- first TxCmdReturnCollateralValidationError $ validateTxReturnCollateral era mReturnCollateral
    validatedFee
      <- first TxCmdTxFeeValidationError $ validateTxFee era mFee
    validatedBounds <- (,) <$> first TxCmdTxValidityLowerBoundValidationError (validateTxValidityLowerBound era mLowerBound)
                           <*> first TxCmdTxValidityUpperBoundValidationError (validateTxValidityUpperBound era mUpperBound)
    validatedReqSigners
      <- first TxCmdRequiredSignersValidationError $ validateRequiredSigners era reqSigners
    validatedPParams
      <- first TxCmdProtocolParametersValidationError $ validateProtocolParameters era mpparams
    validatedTxWtdrwls
      <- first TxCmdTxWithdrawalsValidationError $ validateTxWithdrawals era withdrawals
    validatedTxCerts
      <- first TxCmdTxCertificatesValidationError $ validateTxCertificates era certsAndMaybeSriptWits
    validatedTxUpProp
      <- first TxCmdTxUpdateProposalValidationError $ validateTxUpdateProposal era mUpdateProp
    validatedMintValue
      <- createTxMintValue era valuesWithScriptWits
    validatedTxScriptValidity
      <- first TxCmdScriptValidityValidationError $ validateTxScriptValidity era mScriptValidity
    let validatedTxGovernanceActions = TxGovernanceActionsNone -- TODO: Conwary era
        validatedTxVotes = TxVotesNone -- TODO: Conwary era
    let txBodyContent = TxBodyContent
                          (validateTxIns inputsAndMaybeScriptWits)
                          validatedCollateralTxIns
                          validatedRefInputs
                          txouts
                          validatedTotCollateral
                          validatedRetCol
                          validatedFee
                          validatedBounds
                          txMetadata
                          txAuxScripts
                          validatedReqSigners
                          validatedPParams
                          validatedTxWtdrwls
                          validatedTxCerts
                          validatedTxUpProp
                          validatedMintValue
                          validatedTxScriptValidity
                          validatedTxGovernanceActions
                          validatedTxVotes

    first TxCmdTxBodyError $
      getIsCardanoEraConstraint era $ createAndValidateTransactionBody txBodyContent

runTxBuild
  :: SocketPath
  -> CardanoEra era
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
  -> TxVotes era
  -> TxGovernanceActions era
  -> TxBuildOutputOptions
  -> ExceptT TxCmdError IO (BalancedTxBody era)
runTxBuild
    socketPath era (AnyConsensusModeParams cModeParams) networkId mScriptValidity
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
    <- hoistEither $ first TxCmdTotalCollateralValidationError $ validateTxTotalCollateral era mTotCollateral
  validatedRetCol
    <- hoistEither $ first TxCmdReturnCollateralValidationError $ validateTxReturnCollateral era mReturnCollateral
  dFee <- hoistEither $ first TxCmdTxFeeValidationError $ validateTxFee era dummyFee
  validatedBounds <- (,) <$> hoistEither (first TxCmdTxValidityLowerBoundValidationError $ validateTxValidityLowerBound era mLowerBound)
                         <*> hoistEither (first TxCmdTxValidityUpperBoundValidationError $ validateTxValidityUpperBound era mUpperBound)
  validatedReqSigners <- hoistEither (first TxCmdRequiredSignersValidationError $ validateRequiredSigners era reqSigners)
  validatedTxWtdrwls <- hoistEither (first TxCmdTxWithdrawalsValidationError $ validateTxWithdrawals era withdrawals)
  validatedTxCerts <- hoistEither (first TxCmdTxCertificatesValidationError $ validateTxCertificates era certsAndMaybeScriptWits)
  validatedTxUpProp <- hoistEither (first TxCmdTxUpdateProposalValidationError $ validateTxUpdateProposal era mUpdatePropF)
  validatedMintValue <- hoistEither $ createTxMintValue era valuesWithScriptWits
  validatedTxScriptValidity <- hoistEither (first TxCmdScriptValidityValidationError $ validateTxScriptValidity era mScriptValidity)

  case (consensusMode, cardanoEraStyle era) of
    (CardanoMode, ShelleyBasedEra _sbe) -> do
      void $ pure (toEraInMode era CardanoMode)
        & onNothing (left (TxCmdEraConsensusModeMismatchTxBalanceError outputOptions
                            (AnyConsensusMode CardanoMode) (AnyCardanoEra era)))

      let allTxInputs = inputsThatRequireWitnessing ++ allReferenceInputs ++ txinsc
          localNodeConnInfo = LocalNodeConnectInfo
                                     { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
                                     , localNodeNetworkId = networkId
                                     , localNodeSocketPath = socketPath
                                     }

      AnyCardanoEra nodeEra <- lift (executeLocalStateQueryExpr localNodeConnInfo Nothing (determineEraExpr cModeParams))
        & onLeft (left . TxCmdQueryConvenienceError . AcqFailure)
        & onLeft (left . TxCmdQueryConvenienceError . QceUnsupportedNtcVersion)

      let certs =
            case validatedTxCerts of
              TxCertificates _ cs _ -> cs
              _ -> []

      nodeEraCerts <- pure (forM certs $ eraCast nodeEra)
        & onLeft (left . TxCmdTxEraCastError)

      (nodeEraUTxO, pparams, eraHistory, systemStart, stakePools, stakeDelegDeposits) <-
        lift (executeLocalStateQueryExpr localNodeConnInfo Nothing $ queryStateForBalancedTx nodeEra allTxInputs nodeEraCerts)
          & onLeft (left . TxCmdQueryConvenienceError . AcqFailure)
          & onLeft (left . TxCmdQueryConvenienceError)

      validatedPParams <- hoistEither $ first TxCmdProtocolParametersValidationError
                                      $ validateProtocolParameters era (Just pparams)
      let validatedTxGovernanceActions = proposals
          validatedTxVotes =  votes
          txBodyContent = TxBodyContent
                          (validateTxIns inputsAndMaybeScriptWits)
                          validatedCollateralTxIns
                          validatedRefInputs
                          txouts
                          validatedTotCollateral
                          validatedRetCol
                          dFee
                          validatedBounds
                          txMetadata
                          txAuxScripts
                          validatedReqSigners
                          validatedPParams
                          validatedTxWtdrwls
                          validatedTxCerts
                          validatedTxUpProp
                          validatedMintValue
                          validatedTxScriptValidity
                          validatedTxGovernanceActions
                          validatedTxVotes

      firstExceptT TxCmdTxInsDoNotExistError
        . hoistEither $ txInsExistInUTxO allTxInputs nodeEraUTxO
      firstExceptT TxCmdQueryNotScriptLockedError
        . hoistEither $ notScriptLockedTxIns txinsc nodeEraUTxO

      cAddr <- pure (anyAddressInEra era changeAddr)
        & onLeft (error $ "runTxBuild: Byron address used: " <> show changeAddr) -- should this throw instead?

      -- Why do we cast the era? The user can specify an era prior to the era that the node is currently in.
      -- We cannot use the user specified era to construct a query against a node because it may differ
      -- from the node's era and this will result in the 'QueryEraMismatch' failure.
      txEraUtxo <- pure (eraCast era nodeEraUTxO) & onLeft (left . TxCmdTxEraCastError)

      balancedTxBody@(BalancedTxBody _ _ _ fee) <-
        firstExceptT TxCmdBalanceTxBodyError
          . hoistEither
          $ makeTransactionBodyAutoBalance systemStart (toLedgerEpochInfo eraHistory)
                                           pparams stakePools stakeDelegDeposits txEraUtxo
                                           txBodyContent cAddr mOverrideWits

      liftIO $ putStrLn $ "Estimated transaction fee: " <> (show fee :: String)

      return balancedTxBody

    (CardanoMode, LegacyByronEra) -> left TxCmdByronEraInvalidError

    (wrongMode, _) -> left (TxCmdUnsupportedModeError (AnyConsensusMode wrongMode))

-- ----------------------------------------------------------------------------
-- Transaction body validation and conversion
--

validateTxIns
  :: [(TxIn, Maybe (ScriptWitness WitCtxTxIn era))]
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


validateTxInsCollateral :: CardanoEra era
                        -> [TxIn]
                        -> Either TxCmdError (TxInsCollateral era)
validateTxInsCollateral _   []    = return TxInsCollateralNone
validateTxInsCollateral era txins =
    case collateralSupportedInEra era of
      Nothing -> txFeatureMismatchPure era TxFeatureCollateral
      Just supported -> return (TxInsCollateral supported txins)

validateTxInsReference
  :: CardanoEra era
  -> [TxIn]
  -> Either TxCmdError (TxInsReference BuildTx era)
validateTxInsReference _ []  = return TxInsReferenceNone
validateTxInsReference era allRefIns =
  case Api.refInsScriptsAndInlineDatsSupportedInEra era of
    Nothing -> txFeatureMismatchPure era TxFeatureReferenceInputs
    Just supp -> return $ TxInsReference supp allRefIns


getAllReferenceInputs
 :: [(TxIn, Maybe (ScriptWitness WitCtxTxIn era))]
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
      PlutusScriptWitness _ _ (Api.PReferenceScript refIn _) _ _ _ -> Just refIn
      PlutusScriptWitness _ _ Api.PScript{} _ _ _ -> Nothing
      SimpleScriptWitness _ (Api.SReferenceScript refIn _)  -> Just refIn
      SimpleScriptWitness _ Api.SScript{}  -> Nothing

toAddressInAnyEra
  :: CardanoEra era
  -> AddressAny
  -> Either TxCmdError (AddressInEra era)
toAddressInAnyEra era addrAny = runExcept $ do
  case addrAny of
    AddressByron   bAddr -> pure (AddressInEra ByronAddressInAnyEra bAddr)
    AddressShelley sAddr -> do
      sbe <- requireShelleyBasedEra era
        & onNothing (txFeatureMismatch era TxFeatureShelleyAddresses)

      pure (AddressInEra (ShelleyAddressInEra sbe) sAddr)

toTxOutValueInAnyEra
  :: CardanoEra era
  -> Value
  -> Either TxCmdError (TxOutValue era)
toTxOutValueInAnyEra era val =
  case multiAssetSupportedInEra era of
    Left adaOnlyInEra ->
      case valueToLovelace val of
        Just l  -> return (TxOutAdaOnly adaOnlyInEra l)
        Nothing -> txFeatureMismatchPure era TxFeatureMultiAssetOutputs
    Right multiAssetInEra -> return (TxOutValue multiAssetInEra val)

toTxOutInAnyEra :: CardanoEra era
                -> TxOutAnyEra
                -> ExceptT TxCmdError IO (TxOut CtxTx era)
toTxOutInAnyEra era (TxOutAnyEra addr' val' mDatumHash refScriptFp) = do
  addr <- hoistEither $ toAddressInAnyEra era addr'
  val <- hoistEither $ toTxOutValueInAnyEra era val'
  (datum, refScript)
    <- case (scriptDataSupportedInEra era, Api.refInsScriptsAndInlineDatsSupportedInEra era) of
         (Nothing, Nothing) -> pure (TxOutDatumNone, Api.ReferenceScriptNone)
         (Just sup, Nothing)->
           (,) <$> toTxAlonzoDatum sup mDatumHash <*> pure Api.ReferenceScriptNone
         (Just sup, Just inlineDatumRefScriptSupp) ->
           toTxDatumReferenceScriptBabbage sup inlineDatumRefScriptSupp mDatumHash refScriptFp
         (Nothing, Just _) ->
           -- TODO: Figure out how to make this state unrepresentable
           error "toTxOutInAnyEra: Should not be possible that inline datums are allowed but datums are not"
  pure $ TxOut addr val datum refScript
 where
  getReferenceScript
    :: ReferenceScriptAnyEra
    -> Api.ReferenceTxInsScriptsInlineDatumsSupportedInEra era
    -> ExceptT TxCmdError IO (Api.ReferenceScript era)
  getReferenceScript ReferenceScriptAnyEraNone _ = return Api.ReferenceScriptNone
  getReferenceScript (ReferenceScriptAnyEra fp) supp = do
    Api.ReferenceScript supp
      <$> firstExceptT TxCmdScriptFileError (readFileScriptInAnyLang fp)

  toTxDatumReferenceScriptBabbage
    :: ScriptDataSupportedInEra era
    -> Api.ReferenceTxInsScriptsInlineDatumsSupportedInEra era
    -> TxOutDatumAnyEra
    -> ReferenceScriptAnyEra
    -> ExceptT TxCmdError IO (TxOutDatum CtxTx era, Api.ReferenceScript era)
  toTxDatumReferenceScriptBabbage sDataSupp inlineRefSupp cliDatum refScriptFp' = do
    refScript <- getReferenceScript refScriptFp' inlineRefSupp
    case cliDatum of
       TxOutDatumByNone -> do
         pure (TxOutDatumNone, refScript)
       TxOutDatumByHashOnly dh -> do
         pure (TxOutDatumHash sDataSupp dh, refScript)
       TxOutDatumByHashOf fileOrSdata -> do
         sData <- firstExceptT TxCmdScriptDataError
                    $ readScriptDataOrFile fileOrSdata
         pure (TxOutDatumHash sDataSupp $ hashScriptDataBytes sData, refScript)
       TxOutDatumByValue fileOrSdata -> do
         sData <- firstExceptT TxCmdScriptDataError
                    $ readScriptDataOrFile fileOrSdata
         pure (TxOutDatumInTx sDataSupp sData, refScript)
       TxOutInlineDatumByValue fileOrSdata -> do
         sData <- firstExceptT TxCmdScriptDataError
                    $ readScriptDataOrFile fileOrSdata
         pure (TxOutDatumInline inlineRefSupp sData, refScript)

  toTxAlonzoDatum
    :: ScriptDataSupportedInEra era
    -> TxOutDatumAnyEra
    -> ExceptT TxCmdError IO (TxOutDatum CtxTx era)
  toTxAlonzoDatum supp cliDatum =
    case cliDatum of
      TxOutDatumByHashOnly h -> pure (TxOutDatumHash supp h)
      TxOutDatumByHashOf sDataOrFile -> do
        sData <- firstExceptT TxCmdScriptDataError
                   $ readScriptDataOrFile sDataOrFile
        pure (TxOutDatumHash supp $ hashScriptDataBytes sData)
      TxOutDatumByValue sDataOrFile -> do
        sData <- firstExceptT TxCmdScriptDataError
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
createTxMintValue :: forall era. CardanoEra era
                  -> (Value, [ScriptWitness WitCtxMint era])
                  -> Either TxCmdError (TxMintValue BuildTx era)
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
    | otherwise = Left (TxCmdPolicyIdsMissingError witnessesMissing)
    where
      witnessesMissing = Set.elems (witnessesNeeded Set.\\ witnessesProvided)

  validateNoUnnecessaryWitnesses witnessesNeeded witnessesProvided
    | null witnessesExtra = return ()
    | otherwise = Left (TxCmdPolicyIdsExcessError witnessesExtra)
    where
      witnessesExtra = Set.elems (witnessesProvided Set.\\ witnessesNeeded)

scriptWitnessPolicyId :: ScriptWitness witctx era -> Maybe PolicyId
scriptWitnessPolicyId = \case
  SimpleScriptWitness _ (Api.SScript script) ->
    Just . scriptPolicyId $ SimpleScript script
  SimpleScriptWitness _ (Api.SReferenceScript _ mPid) ->
    PolicyId <$> mPid
  PlutusScriptWitness _ version (Api.PScript script) _ _ _ ->
    Just . scriptPolicyId $ PlutusScript version script
  PlutusScriptWitness _ _ (Api.PReferenceScript _ mPid) _ _ _ ->
    PolicyId <$> mPid

readValueScriptWitnesses
  :: CardanoEra era
  -> (Value, [ScriptWitnessFiles WitCtxMint])
  -> ExceptT TxCmdError IO (Value, [ScriptWitness WitCtxMint era])
readValueScriptWitnesses era (v, sWitFiles) = do
  sWits <- mapM (firstExceptT TxCmdScriptWitnessError . readScriptWitness era) sWitFiles
  return (v, sWits)

-- ----------------------------------------------------------------------------
-- Transaction signing
--

runTxSign :: InputTxBodyOrTxFile
          -> [WitnessSigningData]
          -> Maybe NetworkId
          -> TxFile Out
          -> ExceptT TxCmdError IO ()
runTxSign txOrTxBody witSigningData mnw outTxFile = do
  sks <-  mapM (firstExceptT TxCmdReadWitnessSigningDataError . newExceptT . readWitnessSigningData) witSigningData

  let (sksByron, sksShelley) = partitionSomeWitnesses $ map categoriseSomeWitness sks

  case txOrTxBody of
    InputTxFile (File inputTxFilePath) -> do
      inputTxFile <- liftIO $ fileOrPipe inputTxFilePath
      anyTx <- lift (readFileTx inputTxFile) & onLeft (left . TxCmdCddlError)

      InAnyShelleyBasedEra _era tx <-
          onlyInShelleyBasedEras "sign for Byron era transactions" anyTx

      let (txbody, existingTxKeyWits) = Api.getTxBodyAndWitnesses tx

      byronWitnesses <- firstExceptT TxCmdBootstrapWitnessError
                          . hoistEither
                          $ mkShelleyBootstrapWitnesses mnw txbody sksByron

      let newShelleyKeyWits = map (makeShelleyKeyWitness txbody) sksShelley
          allKeyWits = existingTxKeyWits ++ newShelleyKeyWits ++ byronWitnesses
          signedTx = makeSignedTransaction allKeyWits txbody

      lift (writeTxFileTextEnvelopeCddl outTxFile signedTx)
        & onLeft (left . TxCmdWriteFileError)

    InputTxBodyFile (File txbodyFilePath) -> do
      txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
      unwitnessed <- firstExceptT TxCmdCddlError . newExceptT
                       $ readFileTxBody txbodyFile

      case unwitnessed of
        IncompleteCddlFormattedTx anyTx -> do
         InAnyShelleyBasedEra _era unwitTx <-
           onlyInShelleyBasedEras "sign for Byron era transactions" anyTx

         let txbody = getTxBody unwitTx
         -- Byron witnesses require the network ID. This can either be provided
         -- directly or derived from a provided Byron address.
         byronWitnesses <- firstExceptT TxCmdBootstrapWitnessError
           . hoistEither
           $ mkShelleyBootstrapWitnesses mnw txbody sksByron

         let shelleyKeyWitnesses = map (makeShelleyKeyWitness txbody) sksShelley
             tx = makeSignedTransaction (byronWitnesses ++ shelleyKeyWitnesses) txbody

         lift (writeTxFileTextEnvelopeCddl outTxFile tx)
            & onLeft (left . TxCmdWriteFileError)

        UnwitnessedCliFormattedTxBody anyTxbody -> do
          InAnyShelleyBasedEra _era txbody <-
            --TODO: in principle we should be able to support Byron era txs too
            onlyInShelleyBasedEras "sign for Byron era transactions" anyTxbody
          -- Byron witnesses require the network ID. This can either be provided
          -- directly or derived from a provided Byron address.
          byronWitnesses <- firstExceptT TxCmdBootstrapWitnessError
            . hoistEither
            $ mkShelleyBootstrapWitnesses mnw txbody sksByron

          let shelleyKeyWitnesses = map (makeShelleyKeyWitness txbody) sksShelley
              tx = makeSignedTransaction (byronWitnesses ++ shelleyKeyWitnesses) txbody

          firstExceptT TxCmdWriteFileError . newExceptT
            $ writeLazyByteStringFile outTxFile
            $ textEnvelopeToJSON Nothing tx

-- ----------------------------------------------------------------------------
-- Transaction submission
--


runTxSubmit
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> FilePath
  -> ExceptT TxCmdError IO ()
runTxSubmit socketPath (AnyConsensusModeParams cModeParams) network txFilePath = do
    txFile <- liftIO $ fileOrPipe txFilePath
    InAnyCardanoEra era tx <- lift (readFileTx txFile) & onLeft (left . TxCmdCddlError)
    let cMode = AnyConsensusMode $ consensusModeOnly cModeParams
    eraInMode <- hoistMaybe
                   (TxCmdEraConsensusModeMismatchError (Just txFilePath) cMode (AnyCardanoEra era))
                   (toEraInMode era $ consensusModeOnly cModeParams)
    let txInMode = TxInMode tx eraInMode
        localNodeConnInfo = LocalNodeConnectInfo
                              { localConsensusModeParams = cModeParams
                              , localNodeNetworkId = network
                              , localNodeSocketPath = socketPath
                              }

    res <- liftIO $ submitTxToNodeLocal localNodeConnInfo txInMode
    case res of
      Net.Tx.SubmitSuccess -> liftIO $ Text.putStrLn "Transaction successfully submitted."
      Net.Tx.SubmitFail reason ->
        case reason of
          TxValidationErrorInMode err _eraInMode -> left . TxCmdTxSubmitError . Text.pack $ show err
          TxValidationEraMismatch mismatchErr -> left $ TxCmdTxSubmitErrorEraMismatchError mismatchErr

-- ----------------------------------------------------------------------------
-- Transaction fee calculation
--

runTxCalculateMinFee
  :: TxBodyFile In
  -> NetworkId
  -> ProtocolParamsFile
  -> TxInCount
  -> TxOutCount
  -> TxShelleyWitnessCount
  -> TxByronWitnessCount
  -> ExceptT TxCmdError IO ()
runTxCalculateMinFee (File txbodyFilePath) nw pParamsFile
                     (TxInCount nInputs) (TxOutCount nOutputs)
                     (TxShelleyWitnessCount nShelleyKeyWitnesses)
                     (TxByronWitnessCount nByronKeyWitnesses) = do

    txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
    unwitnessed <- firstExceptT TxCmdCddlError . newExceptT
                     $ readFileTxBody txbodyFile
    pparams <- firstExceptT TxCmdProtocolParamsError $ readProtocolParameters pParamsFile
    case unwitnessed of
      IncompleteCddlFormattedTx anyTx -> do
        InAnyShelleyBasedEra _era unwitTx <-
          onlyInShelleyBasedEras "sign for Byron era transactions" anyTx
        let txbody =  getTxBody unwitTx
        let tx = makeSignedTransaction [] txbody
            Lovelace fee = estimateTransactionFee
                             nw
                             (Api.protocolParamTxFeeFixed pparams)
                             (Api.protocolParamTxFeePerByte pparams)
                             tx
                             nInputs nOutputs
                             nByronKeyWitnesses nShelleyKeyWitnesses

        liftIO $ putStrLn $ (show fee :: String) <> " Lovelace"

      UnwitnessedCliFormattedTxBody anyTxBody -> do
        InAnyShelleyBasedEra _era txbody <-
              --TODO: in principle we should be able to support Byron era txs too
              onlyInShelleyBasedEras "calculate-min-fee for Byron era transactions" anyTxBody

        let tx = makeSignedTransaction [] txbody
            Lovelace fee = estimateTransactionFee
                             nw
                             (Api.protocolParamTxFeeFixed pparams)
                             (Api.protocolParamTxFeePerByte pparams)
                             tx
                             nInputs nOutputs
                             nByronKeyWitnesses nShelleyKeyWitnesses

        liftIO $ putStrLn $ (show fee :: String) <> " Lovelace"

-- ----------------------------------------------------------------------------
-- Transaction fee calculation
--

runTxCalculateMinRequiredUTxO
  :: AnyCardanoEra
  -> ProtocolParamsFile
  -> TxOutAnyEra
  -> ExceptT TxCmdError IO ()
runTxCalculateMinRequiredUTxO (AnyCardanoEra era) pParamsFile txOut = do
  pp <- firstExceptT TxCmdProtocolParamsError (readProtocolParameters pParamsFile)
  out <- toTxOutInAnyEra era txOut
  case cardanoEraStyle era of
    LegacyByronEra -> error "runTxCalculateMinRequiredUTxO: Byron era not implemented yet"
    ShelleyBasedEra sbe -> do
      firstExceptT TxCmdPParamsError . hoistEither
        $ Api.checkProtocolParameters sbe pp
      bpparams <- hoistEither . first (TxCmdTxBodyError . TxBodyProtocolParamsConversionError) $
        bundleProtocolParams era pp
      let minValue = calculateMinimumUTxO sbe out bpparams
      liftIO . IO.print $ minValue

runTxCreatePolicyId :: ScriptFile -> ExceptT TxCmdError IO ()
runTxCreatePolicyId (ScriptFile sFile) = do
  ScriptInAnyLang _ script <- firstExceptT TxCmdScriptFileError $
                                readFileScriptInAnyLang sFile
  liftIO . Text.putStrLn . serialiseToRawBytesHexText $ hashScript script


-- | Error reading the data required to construct a key witness.


partitionSomeWitnesses
  :: [ByronOrShelleyWitness]
  -> ( [ShelleyBootstrapWitnessSigningKeyData]
     , [ShelleyWitnessSigningKey]
     )
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
mkShelleyBootstrapWitness
  :: IsShelleyBasedEra era
  => Maybe NetworkId
  -> TxBody era
  -> ShelleyBootstrapWitnessSigningKeyData
  -> Either BootstrapWitnessError (KeyWitness era)
mkShelleyBootstrapWitness Nothing _ (ShelleyBootstrapWitnessSigningKeyData _ Nothing) =
  Left MissingNetworkIdOrByronAddressError
mkShelleyBootstrapWitness (Just nw) txBody (ShelleyBootstrapWitnessSigningKeyData skey Nothing) =
  Right $ makeShelleyBootstrapWitness (Api.WitnessNetworkId nw) txBody skey
mkShelleyBootstrapWitness _ txBody (ShelleyBootstrapWitnessSigningKeyData skey (Just addr)) =
  Right $ makeShelleyBootstrapWitness (Api.WitnessByronAddress addr) txBody skey

-- | Attempt to construct Shelley bootstrap witnesses until an error is
-- encountered.
mkShelleyBootstrapWitnesses
  :: IsShelleyBasedEra era
  => Maybe NetworkId
  -> TxBody era
  -> [ShelleyBootstrapWitnessSigningKeyData]
  -> Either BootstrapWitnessError [KeyWitness era]
mkShelleyBootstrapWitnesses mnw txBody =
  mapM (mkShelleyBootstrapWitness mnw txBody)


-- ----------------------------------------------------------------------------
-- Other misc small commands
--

runTxHashScriptData :: ScriptDataOrFile -> ExceptT TxCmdError IO ()
runTxHashScriptData scriptDataOrFile = do
    d <- firstExceptT TxCmdScriptDataError $ readScriptDataOrFile scriptDataOrFile
    liftIO $ BS.putStrLn $ serialiseToRawBytesHex (hashScriptDataBytes d)

runTxGetTxId :: InputTxBodyOrTxFile -> ExceptT TxCmdError IO ()
runTxGetTxId txfile = do
    InAnyCardanoEra _era txbody <-
      case txfile of
        InputTxBodyFile (File txbodyFilePath) -> do
          txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
          unwitnessed <- firstExceptT TxCmdCddlError . newExceptT
                           $ readFileTxBody txbodyFile
          case unwitnessed of
            UnwitnessedCliFormattedTxBody anyTxBody -> return anyTxBody
            IncompleteCddlFormattedTx (InAnyCardanoEra era tx) ->
              return (InAnyCardanoEra era (getTxBody tx))

        InputTxFile (File txFilePath) -> do
          txFile <- liftIO $ fileOrPipe txFilePath
          InAnyCardanoEra era tx <- lift (readFileTx txFile) & onLeft (left . TxCmdCddlError)
          return . InAnyCardanoEra era $ getTxBody tx

    liftIO $ BS.putStrLn $ serialiseToRawBytesHex (getTxId txbody)

runTxView :: InputTxBodyOrTxFile -> ExceptT TxCmdError IO ()
runTxView = \case
  InputTxBodyFile (File txbodyFilePath) -> do
    txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
    unwitnessed <- firstExceptT TxCmdCddlError . newExceptT
                     $ readFileTxBody txbodyFile
    InAnyCardanoEra era txbody <-
      case unwitnessed of
        UnwitnessedCliFormattedTxBody anyTxBody -> pure anyTxBody
        IncompleteCddlFormattedTx (InAnyCardanoEra era tx) ->
          pure $ InAnyCardanoEra era (getTxBody tx)
    --TODO: Why are we maintaining friendlyTxBodyBS and friendlyTxBS?
    -- In the case of a transaction body, we can simply call makeSignedTransaction []
    -- to get a transaction which allows us to reuse friendlyTxBS!
    liftIO $ BS.putStr $ friendlyTxBodyBS era txbody
  InputTxFile (File txFilePath) -> do
    txFile <- liftIO $ fileOrPipe txFilePath
    InAnyCardanoEra era tx <- lift (readFileTx txFile) & onLeft (left . TxCmdCddlError)
    liftIO $ BS.putStr $ friendlyTxBS era tx


-- ----------------------------------------------------------------------------
-- Witness commands
--

runTxCreateWitness
  :: TxBodyFile In
  -> WitnessSigningData
  -> Maybe NetworkId
  -> File () Out
  -> ExceptT TxCmdError IO ()
runTxCreateWitness (File txbodyFilePath) witSignData mbNw oFile = do
  txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
  unwitnessed <- firstExceptT TxCmdCddlError . newExceptT
                   $ readFileTxBody txbodyFile
  case unwitnessed of
    IncompleteCddlFormattedTx anyTx -> do
     InAnyShelleyBasedEra sbe cddlTx <-
       onlyInShelleyBasedEras "sign for Byron era transactions" anyTx

     let txbody = getTxBody cddlTx
     someWit <- firstExceptT TxCmdReadWitnessSigningDataError
                  . newExceptT $ readWitnessSigningData witSignData
     witness <-
       case categoriseSomeWitness someWit of
         -- Byron witnesses require the network ID. This can either be provided
         -- directly or derived from a provided Byron address.
         AByronWitness bootstrapWitData ->
           firstExceptT TxCmdBootstrapWitnessError
             . hoistEither
             $ mkShelleyBootstrapWitness mbNw txbody bootstrapWitData
         AShelleyKeyWitness skShelley ->
           pure $ makeShelleyKeyWitness txbody skShelley

     firstExceptT TxCmdWriteFileError . newExceptT
       $ writeTxWitnessFileTextEnvelopeCddl sbe oFile witness

    UnwitnessedCliFormattedTxBody anyTxbody -> do
      InAnyShelleyBasedEra _era txbody <-
        onlyInShelleyBasedEras "sign for Byron era transactions" anyTxbody

      someWit <- firstExceptT TxCmdReadWitnessSigningDataError
                   . newExceptT $ readWitnessSigningData witSignData

      witness <-
        case categoriseSomeWitness someWit of
          -- Byron witnesses require the network ID. This can either be provided
          -- directly or derived from a provided Byron address.
          AByronWitness bootstrapWitData ->
            firstExceptT TxCmdBootstrapWitnessError
              . hoistEither
              $ mkShelleyBootstrapWitness mbNw txbody bootstrapWitData
          AShelleyKeyWitness skShelley ->
            pure $ makeShelleyKeyWitness txbody skShelley

      firstExceptT TxCmdWriteFileError . newExceptT
        $ writeLazyByteStringFile oFile
        $ textEnvelopeToJSON Nothing witness

runTxSignWitness
  :: TxBodyFile In
  -> [WitnessFile]
  -> File () Out
  -> ExceptT TxCmdError IO ()
runTxSignWitness (File txbodyFilePath) witnessFiles oFp = do
    txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
    unwitnessed <- firstExceptT TxCmdCddlError . newExceptT
                     $ readFileTxBody txbodyFile
    case unwitnessed of
      UnwitnessedCliFormattedTxBody (InAnyCardanoEra era txbody) -> do
        witnesses <-
          sequence
            [ do InAnyCardanoEra era' witness <- firstExceptT TxCmdCddlWitnessError . newExceptT
                                                   $ readFileTxKeyWitness file
                 case testEquality era era' of
                   Nothing   -> left $ TxCmdWitnessEraMismatchError
                                         (AnyCardanoEra era)
                                         (AnyCardanoEra era')
                                         witnessFile
                   Just Refl -> return witness
            | witnessFile@(WitnessFile file) <- witnessFiles
            ]

        let tx = makeSignedTransaction witnesses txbody
        firstExceptT TxCmdWriteFileError
          . newExceptT
          $ writeLazyByteStringFile oFp
          $ textEnvelopeToJSON Nothing tx

      IncompleteCddlFormattedTx (InAnyCardanoEra era anyTx) -> do
        let txbody = getTxBody anyTx

        witnesses <-
          sequence
            [ do InAnyCardanoEra era' witness <- firstExceptT TxCmdCddlWitnessError . newExceptT
                                                   $ readFileTxKeyWitness file
                 case testEquality era era' of
                   Nothing   -> left $ TxCmdWitnessEraMismatchError
                                         (AnyCardanoEra era)
                                         (AnyCardanoEra era')
                                         witnessFile
                   Just Refl -> return witness
            | witnessFile@(WitnessFile file) <- witnessFiles ]

        let tx = makeSignedTransaction witnesses txbody

        lift (writeTxFileTextEnvelopeCddl oFp tx) & onLeft (left . TxCmdWriteFileError)


-- | Constrain the era to be Shelley based. Fail for the Byron era.
--
onlyInShelleyBasedEras :: Text
                       -> InAnyCardanoEra a
                       -> ExceptT TxCmdError IO (InAnyShelleyBasedEra a)
onlyInShelleyBasedEras notImplMsg (InAnyCardanoEra era x) =
    case cardanoEraStyle era of
      LegacyByronEra      -> left (TxCmdNotImplementedError notImplMsg)
      ShelleyBasedEra sbe -> return (InAnyShelleyBasedEra sbe x)


