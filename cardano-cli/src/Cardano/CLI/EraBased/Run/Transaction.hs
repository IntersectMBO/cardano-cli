{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Cardano.CLI.EraBased.Run.Transaction
  ( runTransactionCmds
  , runTxBuildCmd
  , runTxBuildRawCmd
  , runTxSignCmd
  , runTxSubmitCmd
  , runTxCalculateMinFeeCmd
  , runTxCalculateMinRequiredUTxOCmd
  , runTxCreatePolicyIdCmd
  , runTxHashScriptDataCmd
  , runTxGetTxIdCmd
  , runTxViewCmd
  , runTxCreateWitnessCmd
  , runTxSignWitnessCmd
  ) where

import           Cardano.Api
import           Cardano.Api.Byron hiding (SomeByronSigningKey (..))
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Commands.Transaction
import           Cardano.CLI.EraBased.Run.Genesis
import           Cardano.CLI.Json.Friendly (friendlyTxBodyJson, friendlyTxBodyYaml, friendlyTxJson,
                   friendlyTxYaml)
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.BootstrapWitnessError
import           Cardano.CLI.Types.Errors.TxCmdError
import           Cardano.CLI.Types.Errors.TxValidationError
import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Output (renderScriptCosts)
import           Cardano.CLI.Types.TxFeature
import qualified Cardano.Ledger.Alonzo.Core as Ledger
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
import           Lens.Micro ((^.))
import qualified System.IO as IO


runTransactionCmds :: TransactionCmds era -> ExceptT TxCmdError IO ()
runTransactionCmds cmd =
  case cmd of
    TxBuild
        era mNodeSocketPath consensusModeParams nid mScriptValidity mOverrideWits txins readOnlyRefIns
        reqSigners txinsc mReturnColl mTotCollateral txouts changeAddr mValue mLowBound
        mUpperBound certs wdrls metadataSchema scriptFiles metadataFiles mUpProp voteFiles
        proposalFiles outputOptions ->
      runTxBuildCmd
        era mNodeSocketPath consensusModeParams nid mScriptValidity mOverrideWits txins readOnlyRefIns
        reqSigners txinsc mReturnColl mTotCollateral txouts changeAddr mValue mLowBound
        mUpperBound certs wdrls metadataSchema scriptFiles metadataFiles mUpProp voteFiles
        proposalFiles outputOptions
    TxBuildRaw
        era mScriptValidity txins readOnlyRefIns txinsc mReturnColl
        mTotColl reqSigners txouts mValue mLowBound mUpperBound fee certs wdrls
        metadataSchema scriptFiles metadataFiles mProtocolParamsFile mUpProp
        voteFiles proposalFiles
        out ->
      runTxBuildRawCmd era mScriptValidity txins readOnlyRefIns txinsc mReturnColl
        mTotColl reqSigners txouts mValue mLowBound mUpperBound fee certs wdrls
        metadataSchema scriptFiles metadataFiles mProtocolParamsFile mUpProp
        voteFiles proposalFiles
        out
    TxSign txinfile skfiles network txoutfile ->
      runTxSignCmd txinfile skfiles network txoutfile
    TxSubmit mNodeSocketPath anyConsensusModeParams network txFp ->
      runTxSubmitCmd mNodeSocketPath anyConsensusModeParams network txFp
    TxCalculateMinFee txbody nw pParamsFile nInputs nOutputs nShelleyKeyWitnesses nByronKeyWitnesses ->
      runTxCalculateMinFeeCmd txbody nw pParamsFile nInputs nOutputs nShelleyKeyWitnesses nByronKeyWitnesses
    TxCalculateMinRequiredUTxO era pParamsFile txOuts ->
      runTxCalculateMinRequiredUTxOCmd era pParamsFile txOuts
    TxHashScriptData scriptDataOrFile ->
      runTxHashScriptDataCmd scriptDataOrFile
    TxGetTxId txinfile ->
      runTxGetTxIdCmd txinfile
    TxView outFormat mOutFile txinfile ->
      runTxViewCmd outFormat mOutFile txinfile
    TxMintedPolicyId sFile ->
      runTxCreatePolicyIdCmd sFile
    TxCreateWitness txBodyfile witSignData mbNw outFile ->
      runTxCreateWitnessCmd txBodyfile witSignData mbNw outFile
    TxAssembleTxBodyWitness txBodyFile witnessFile outFile ->
      runTxSignWitnessCmd txBodyFile witnessFile outFile

-- ----------------------------------------------------------------------------
-- Building transactions
--

runTxBuildCmd :: ()
  => CardanoEra era
  -> SocketPath
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
  -> [ProposalFile In]
  -> TxBuildOutputOptions
  -> ExceptT TxCmdError IO ()
runTxBuildCmd
    era socketPath consensusModeParams@(AnyConsensusModeParams cModeParams) nid
    mScriptValidity mOverrideWits txins readOnlyRefIns reqSigners txinsc mReturnColl mTotCollateral txouts
    changeAddr mValue mLowBound mUpperBound certs wdrls metadataSchema scriptFiles metadataFiles mUpProp
    voteFiles proposalFiles outputOptions = do

  -- The user can specify an era prior to the era that the node is currently in.
  -- We cannot use the user specified era to construct a query against a node because it may differ
  -- from the node's era and this will result in the 'QueryEraMismatch' failure.

  let localNodeConnInfo = LocalNodeConnectInfo
                            { localConsensusModeParams = cModeParams
                            , localNodeNetworkId = nid
                            , localNodeSocketPath = socketPath
                            }

  inputsAndMaybeScriptWits <- firstExceptT TxCmdScriptWitnessError $ readScriptWitnessFiles era txins
  certFilesAndMaybeScriptWits <- firstExceptT TxCmdScriptWitnessError $ readScriptWitnessFiles era certs

  -- TODO: Conway Era - How can we make this more composable?
  certsAndMaybeScriptWits <-
    case cardanoEraStyle era of
      LegacyByronEra -> return []
      ShelleyBasedEra{} ->
        sequence
          [ fmap (,mSwit) (firstExceptT TxCmdReadTextViewFileError . newExceptT $
              readFileTextEnvelope AsCertificate (File certFile))
          | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
          ]
  withdrawalsAndMaybeScriptWits <- firstExceptT TxCmdScriptWitnessError $
    readScriptWitnessFilesThruple era wdrls
  txMetadata <- firstExceptT TxCmdMetadataError . newExceptT $
    readTxMetadata era metadataSchema metadataFiles
  valuesWithScriptWits <- readValueScriptWitnesses era $ fromMaybe mempty mValue
  scripts <- firstExceptT TxCmdScriptFileError $
    mapM (readFileScriptInAnyLang . unScriptFile) scriptFiles
  txAuxScripts <- hoistEither $ first TxCmdAuxScriptsValidationError $ validateTxAuxScripts era scripts

  mProp <- forM mUpProp $ \(UpdateProposalFile upFp) ->
    firstExceptT TxCmdReadTextViewFileError (newExceptT $ readFileTextEnvelope AsUpdateProposal (File upFp))
  requiredSigners  <- mapM (firstExceptT TxCmdRequiredSignerError .  newExceptT . readRequiredSigner) reqSigners
  mReturnCollateral <- forM mReturnColl $ toTxOutInAnyEra era

  txOuts <- mapM (toTxOutInAnyEra era) txouts

  -- Conway related
  votingProcedures <-
    inEonForEra
      (pure emptyVotingProcedures)
      (\w -> firstExceptT TxCmdVoteError $ ExceptT (readVotingProceduresFiles w voteFiles))
      era

  proposals <- newExceptT $ first TxCmdConstitutionError
                  <$> readTxGovernanceActions era proposalFiles

  -- the same collateral input can be used for several plutus scripts
  let filteredTxinsc = Set.toList $ Set.fromList txinsc

  -- We need to construct the txBodycontent outside of runTxBuild
  BalancedTxBody txBodyContent balancedTxBody _ _ <-
    runTxBuild
      era socketPath consensusModeParams nid mScriptValidity inputsAndMaybeScriptWits readOnlyRefIns filteredTxinsc
      mReturnCollateral mTotCollateral txOuts changeAddr valuesWithScriptWits mLowBound
      mUpperBound certsAndMaybeScriptWits withdrawalsAndMaybeScriptWits
      requiredSigners txAuxScripts txMetadata mProp mOverrideWits votingProcedures proposals outputOptions

  let mScriptWits =
        forEraInEon era [] $ \sbe -> collectTxBodyScriptWitnesses sbe txBodyContent

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
      let BuildTxWith mTxProtocolParams = txProtocolParams txBodyContent

      pparams <- pure mTxProtocolParams & onNothing (left TxCmdProtocolParametersNotPresentInTxBody)
      executionUnitPrices <- pure (getExecutionUnitPrices era pparams) & onNothing (left TxCmdPParamExecutionUnitsNotAvailable)
      let consensusMode = consensusModeOnly cModeParams

      case consensusMode of
        CardanoMode -> do
          AnyCardanoEra nodeEra <- lift (executeLocalStateQueryExpr localNodeConnInfo Nothing (determineEraExpr cModeParams))
            & onLeft (left . TxCmdQueryConvenienceError . AcqFailure)
            & onLeft (left . TxCmdQueryConvenienceError . QceUnsupportedNtcVersion)

          (nodeEraUTxO, _, eraHistory, systemStart, _, _, _) <-
            lift (executeLocalStateQueryExpr localNodeConnInfo Nothing (queryStateForBalancedTx nodeEra allTxInputs []))
              & onLeft (left . TxCmdQueryConvenienceError . AcqFailure)
              & onLeft (left . TxCmdQueryConvenienceError)

          -- Why do we cast the era? The user can specify an era prior to the era that the node is currently in.
          -- We cannot use the user specified era to construct a query against a node because it may differ
          -- from the node's era and this will result in the 'QueryEraMismatch' failure.
          txEraUtxo <- cardanoEraConstraints era $ pure (eraCast era nodeEraUTxO) & onLeft (left . TxCmdTxEraCastErr)

          scriptExecUnitsMap <-
            firstExceptT TxCmdTxExecUnitsErr $ hoistEither
              $ evaluateTransactionExecutionUnits
                  systemStart (toLedgerEpochInfo eraHistory)
                  pparams txEraUtxo balancedTxBody

          scriptCostOutput <-
            firstExceptT TxCmdPlutusScriptCostErr $ hoistEither
              $ renderScriptCosts
                  txEraUtxo
                  executionUnitPrices
                  mScriptWits
                  scriptExecUnitsMap
          liftIO $ LBS.writeFile (unFile fp) $ encodePretty scriptCostOutput
        _ -> left TxCmdPlutusScriptsRequireCardanoMode

    OutputTxBodyOnly fpath ->
      let noWitTx = makeSignedTransaction [] balancedTxBody
      in  lift (cardanoEraConstraints era $ writeTxFileTextEnvelopeCddl fpath noWitTx)
            & onLeft (left . TxCmdWriteFileError)

getExecutionUnitPrices :: CardanoEra era -> LedgerProtocolParameters era -> Maybe Ledger.Prices
getExecutionUnitPrices cEra (LedgerProtocolParameters pp) = do
  ShelleyBasedEra sbe <- pure $ cardanoEraStyle cEra
  case sbe of
    ShelleyBasedEraShelley -> Nothing
    ShelleyBasedEraAllegra -> Nothing
    ShelleyBasedEraMary -> Nothing
    ShelleyBasedEraAlonzo -> Just $ pp ^. Ledger.ppPricesL
    ShelleyBasedEraBabbage -> Just $ pp ^. Ledger.ppPricesL
    ShelleyBasedEraConway -> Just $ pp ^. Ledger.ppPricesL

runTxBuildRawCmd
  :: CardanoEra era
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
  -> [VoteFile In]
  -> [ProposalFile In]
  -> TxBodyFile Out
  -> ExceptT TxCmdError IO ()
runTxBuildRawCmd
  era mScriptValidity txins readOnlyRefIns txinsc mReturnColl
  mTotColl reqSigners txouts mValue mLowBound mUpperBound fee certs wdrls
  metadataSchema scriptFiles metadataFiles mpParamsFile mUpProp
  voteFiles proposalFiles out = do
  inputsAndMaybeScriptWits <- firstExceptT TxCmdScriptWitnessError
                                $ readScriptWitnessFiles era txins
  certFilesAndMaybeScriptWits <- firstExceptT TxCmdScriptWitnessError
                                   $ readScriptWitnessFiles era certs

  -- TODO: Conway era - How can we make this more composable?
  certsAndMaybeScriptWits <-
      case cardanoEraStyle era of
        LegacyByronEra -> return []
        ShelleyBasedEra{} ->
          sequence
            [ fmap (,mSwit) (firstExceptT TxCmdReadTextViewFileError . newExceptT $
                readFileTextEnvelope AsCertificate (File certFile))
            | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
            ]

  withdrawalsAndMaybeScriptWits <- firstExceptT TxCmdScriptWitnessError
                                     $ readScriptWitnessFilesThruple era wdrls
  txMetadata <- firstExceptT TxCmdMetadataError
                  . newExceptT $ readTxMetadata era metadataSchema metadataFiles
  valuesWithScriptWits <- readValueScriptWitnesses era $ fromMaybe mempty mValue
  scripts <- firstExceptT TxCmdScriptFileError $
                     mapM (readFileScriptInAnyLang . unScriptFile) scriptFiles
  txAuxScripts <- hoistEither $ first TxCmdAuxScriptsValidationError $ validateTxAuxScripts era scripts

  -- TODO: Conway era - update readProtocolParameters to rely on Ledger.PParams JSON instances
  pparams <- forM mpParamsFile $ \ppf ->
    firstExceptT TxCmdProtocolParamsError (readProtocolParameters ppf)

  mLedgerPParams <- case cardanoEraStyle era of
    LegacyByronEra -> return Nothing
    ShelleyBasedEra sbe ->
      forM pparams $ \pp ->
        firstExceptT TxCmdProtocolParamsConverstionError
         . hoistEither $ convertToLedgerProtocolParameters sbe pp

  mProp <- forM mUpProp $ \(UpdateProposalFile upFp) ->
    firstExceptT TxCmdReadTextViewFileError (newExceptT $ readFileTextEnvelope AsUpdateProposal (File upFp))

  requiredSigners  <- mapM (firstExceptT TxCmdRequiredSignerError .  newExceptT . readRequiredSigner) reqSigners
  mReturnCollateral <- forM mReturnColl $ toTxOutInAnyEra era
  txOuts <- mapM (toTxOutInAnyEra era) txouts

    -- the same collateral input can be used for several plutus scripts
  let filteredTxinsc = Set.toList $ Set.fromList txinsc

  -- Conway related
  votingProcedures <-
    inEonForEra
      (pure emptyVotingProcedures)
      (\w -> firstExceptT TxCmdVoteError $ ExceptT (readVotingProceduresFiles w voteFiles))
      era

  proposals <-
    lift (readTxGovernanceActions era proposalFiles)
      & onLeft (left . TxCmdConstitutionError)

  txBody <- hoistEither $ runTxBuildRaw era mScriptValidity inputsAndMaybeScriptWits readOnlyRefIns filteredTxinsc
                          mReturnCollateral mTotColl txOuts mLowBound mUpperBound fee valuesWithScriptWits
                          certsAndMaybeScriptWits withdrawalsAndMaybeScriptWits requiredSigners txAuxScripts
                          txMetadata mLedgerPParams mProp votingProcedures proposals

  let noWitTx = makeSignedTransaction [] txBody
  lift (cardanoEraConstraints era $ writeTxFileTextEnvelopeCddl out noWitTx)
    & onLeft (left . TxCmdWriteFileError)


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
  -> Maybe (LedgerProtocolParameters era)
  -> Maybe UpdateProposal
  -> VotingProcedures era
  -> [Proposal era]
  -> Either TxCmdError (TxBody era)
runTxBuildRaw era
              mScriptValidity inputsAndMaybeScriptWits
              readOnlyRefIns txinsc
              mReturnCollateral mTotCollateral txouts
              mLowerBound mUpperBound
              mFee valuesWithScriptWits
              certsAndMaybeSriptWits withdrawals reqSigners
              txAuxScripts txMetadata mpparams mUpdateProp votingProcedures proposals = do

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
    let validatedTxProposal = proposals
        validatedTxVotes = votingProcedures
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
            , txProposalProcedures = forEraInEonMaybe era (`Featured` validatedTxProposal)
            , txVotingProcedures = forEraInEonMaybe era (`Featured` validatedTxVotes)
            }

    first TxCmdTxBodyError $
      cardanoEraConstraints era $ createAndValidateTransactionBody txBodyContent

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
  -> [Proposal era]
  -> TxBuildOutputOptions
  -> ExceptT TxCmdError IO (BalancedTxBody era)
runTxBuild
    era socketPath (AnyConsensusModeParams cModeParams) networkId mScriptValidity
    inputsAndMaybeScriptWits readOnlyRefIns txinsc mReturnCollateral mTotCollateral txouts
    (TxOutChangeAddress changeAddr) valuesWithScriptWits mLowerBound mUpperBound
    certsAndMaybeScriptWits withdrawals reqSigners txAuxScripts txMetadata
    mUpdatePropF mOverrideWits votingProcedures proposals outputOptions = do

  let consensusMode = consensusModeOnly cModeParams
      dummyFee = Just $ Lovelace 0
      inputsThatRequireWitnessing = [input | (input,_) <- inputsAndMaybeScriptWits]

  let allReferenceInputs = getAllReferenceInputs
                             inputsAndMaybeScriptWits
                             (snd valuesWithScriptWits)
                             certsAndMaybeScriptWits
                             withdrawals
                             readOnlyRefIns

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
    (CardanoMode, ShelleyBasedEra _) -> do
      _ <- toEraInMode era CardanoMode
            & hoistMaybe (TxCmdEraConsensusModeMismatchTxBalance outputOptions (AnyConsensusMode CardanoMode) (AnyCardanoEra era))

      let allTxInputs = inputsThatRequireWitnessing ++ allReferenceInputs ++ txinsc
          localNodeConnInfo = LocalNodeConnectInfo
                                     { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
                                     , localNodeNetworkId = networkId
                                     , localNodeSocketPath = socketPath
                                     }

      AnyCardanoEra nodeEra <- lift (executeLocalStateQueryExpr localNodeConnInfo Nothing (determineEraExpr cModeParams))
        & onLeft (left . TxCmdQueryConvenienceError . AcqFailure)
        & onLeft (left . TxCmdQueryConvenienceError . QceUnsupportedNtcVersion)

      Refl <- testEquality era nodeEra
              & hoistMaybe (TxCmdTxEraCastErr $ EraCastError ("nodeEra" :: Text) era nodeEra)

      let certs =
            case validatedTxCerts of
              TxCertificates _ cs _ -> cs
              _ -> []

      (txEraUtxo, pparams, eraHistory, systemStart, stakePools, stakeDelegDeposits, drepDelegDeposits) <-
        lift (executeLocalStateQueryExpr localNodeConnInfo Nothing $ queryStateForBalancedTx nodeEra allTxInputs certs)
          & onLeft (left . TxCmdQueryConvenienceError . AcqFailure)
          & onLeft (left . TxCmdQueryConvenienceError)

      validatedPParams <- hoistEither $ first TxCmdProtocolParametersValidationError
                                      $ validateProtocolParameters era (Just pparams)

      let validatedTxProposalProcedures = proposals
          validatedTxVotes = votingProcedures
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
              , txProposalProcedures = forEraInEonMaybe era (`Featured` validatedTxProposalProcedures)
              , txVotingProcedures = forEraInEonMaybe era (`Featured` validatedTxVotes)
              }

      firstExceptT TxCmdTxInsDoNotExist
        . hoistEither $ txInsExistInUTxO allTxInputs txEraUtxo
      firstExceptT TxCmdQueryNotScriptLocked
        . hoistEither $ notScriptLockedTxIns txinsc txEraUtxo

      cAddr <- pure (anyAddressInEra era changeAddr)
        & onLeft (error $ "runTxBuild: Byron address used: " <> show changeAddr) -- should this throw instead?

      balancedTxBody@(BalancedTxBody _ _ _ fee) <-
        firstExceptT TxCmdBalanceTxBody
          . hoistEither
          $ makeTransactionBodyAutoBalance systemStart (toLedgerEpochInfo eraHistory)
                                           pparams stakePools stakeDelegDeposits drepDelegDeposits
                                           txEraUtxo txBodyContent cAddr mOverrideWits

      liftIO $ putStrLn $ "Estimated transaction fee: " <> (show fee :: String)

      return balancedTxBody

    (CardanoMode, LegacyByronEra) -> left TxCmdByronEra

    (wrongMode, _) -> left (TxCmdUnsupportedMode (AnyConsensusMode wrongMode))

-- ----------------------------------------------------------------------------
-- Transaction body validation and conversion
--

txFeatureMismatch :: ()
  => Monad m
  => CardanoEra era
  -> TxFeature
  -> ExceptT TxCmdError m a
txFeatureMismatch era feature =
    hoistEither . Left $ TxCmdTxFeatureMismatch (anyCardanoEra era) feature

txFeatureMismatchPure :: CardanoEra era
                      -> TxFeature
                      -> Either TxCmdError a
txFeatureMismatchPure era feature =
    Left (TxCmdTxFeatureMismatch (anyCardanoEra era) feature)


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
validateTxInsCollateral era txins = do
  supported <- forEraMaybeEon era
    & maybe (txFeatureMismatchPure era TxFeatureCollateral) Right
  pure $ TxInsCollateral supported txins

validateTxInsReference
  :: CardanoEra era
  -> [TxIn]
  -> Either TxCmdError (TxInsReference BuildTx era)
validateTxInsReference _ []  = return TxInsReferenceNone
validateTxInsReference era allRefIns = do
  supported <- forEraMaybeEon era
    & maybe (txFeatureMismatchPure era TxFeatureReferenceInputs) Right
  pure $ TxInsReference supported allRefIns

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
      PlutusScriptWitness _ _ (PReferenceScript refIn _) _ _ _ -> Just refIn
      PlutusScriptWitness _ _ PScript{} _ _ _ -> Nothing
      SimpleScriptWitness _ (SReferenceScript refIn _)  -> Just refIn
      SimpleScriptWitness _ SScript{}  -> Nothing

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
  caseByronToAllegraOrMaryEraOnwards
    (\w ->
      case valueToLovelace val of
        Just l  -> return (TxOutAdaOnly w l)
        Nothing -> txFeatureMismatchPure era TxFeatureMultiAssetOutputs
    )
    (\w -> return (TxOutValue w val))
    era

toTxOutInAnyEra :: CardanoEra era
                -> TxOutAnyEra
                -> ExceptT TxCmdError IO (TxOut CtxTx era)
toTxOutInAnyEra era (TxOutAnyEra addr' val' mDatumHash refScriptFp) = do
  addr <- hoistEither $ toAddressInAnyEra era addr'
  val <- hoistEither $ toTxOutValueInAnyEra era val'
  (datum, refScript)
    <- caseByronToMaryOrAlonzoEraOnwards
         (const $ pure (TxOutDatumNone, ReferenceScriptNone))
         (\w ->
            caseAlonzoOnlyOrBabbageEraOnwards
              (\wa ->
                (,)
                  <$> toTxAlonzoDatum (alonzoEraOnlyToAlonzoEraOnwards wa) mDatumHash
                  <*> pure ReferenceScriptNone
              )
              (\wbo -> toTxDatumReferenceScriptBabbage w wbo mDatumHash refScriptFp)
              w
         )
         era

  pure $ TxOut addr val datum refScript
  where
    getReferenceScript :: ()
      => ReferenceScriptAnyEra
      -> BabbageEraOnwards era
      -> ExceptT TxCmdError IO (ReferenceScript era)
    getReferenceScript ReferenceScriptAnyEraNone _ = return ReferenceScriptNone
    getReferenceScript (ReferenceScriptAnyEra fp) supp = do
      ReferenceScript supp
        <$> firstExceptT TxCmdScriptFileError (readFileScriptInAnyLang fp)

    toTxDatumReferenceScriptBabbage :: ()
      => AlonzoEraOnwards era
      -> BabbageEraOnwards era
      -> TxOutDatumAnyEra
      -> ReferenceScriptAnyEra
      -> ExceptT TxCmdError IO (TxOutDatum CtxTx era, ReferenceScript era)
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

    toTxAlonzoDatum :: ()
      => AlonzoEraOnwards era
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
    caseByronToAllegraOrMaryEraOnwards
      (const (txFeatureMismatchPure era TxFeatureMintValue))
      (\w -> do
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

        return (TxMintValue w val (BuildTxWith witnessesProvidedMap))
      )
      era
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
    | otherwise = Left (TxCmdPolicyIdsMissing witnessesMissing)
    where
      witnessesMissing = Set.elems (witnessesNeeded Set.\\ witnessesProvided)

  validateNoUnnecessaryWitnesses witnessesNeeded witnessesProvided
    | null witnessesExtra = return ()
    | otherwise = Left (TxCmdPolicyIdsExcess witnessesExtra)
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

runTxSignCmd :: ()
  => InputTxBodyOrTxFile
  -> [WitnessSigningData]
  -> Maybe NetworkId
  -> TxFile Out
  -> ExceptT TxCmdError IO ()
runTxSignCmd txOrTxBody witSigningData mnw outTxFile = do
  sks <-  mapM (firstExceptT TxCmdReadWitnessSigningDataError . newExceptT . readWitnessSigningData) witSigningData

  let (sksByron, sksShelley) = partitionSomeWitnesses $ map categoriseSomeSigningWitness sks

  case txOrTxBody of
    InputTxFile (File inputTxFilePath) -> do
      inputTxFile <- liftIO $ fileOrPipe inputTxFilePath
      anyTx <- lift (readFileTx inputTxFile) & onLeft (left . TxCmdCddlError)

      InAnyShelleyBasedEra _era tx <-
          onlyInShelleyBasedEras "sign for Byron era transactions" anyTx

      let (txbody, existingTxKeyWits) = getTxBodyAndWitnesses tx

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

runTxSubmitCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> FilePath
  -> ExceptT TxCmdError IO ()
runTxSubmitCmd socketPath (AnyConsensusModeParams cModeParams) network txFilePath = do
    txFile <- liftIO $ fileOrPipe txFilePath
    InAnyCardanoEra era tx <- lift (readFileTx txFile) & onLeft (left . TxCmdCddlError)
    let cMode = AnyConsensusMode $ consensusModeOnly cModeParams
    eraInMode <- hoistMaybe
                   (TxCmdEraConsensusModeMismatch (Just txFilePath) cMode (AnyCardanoEra era))
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
          TxValidationEraMismatch mismatchErr -> left $ TxCmdTxSubmitErrorEraMismatch mismatchErr

-- ----------------------------------------------------------------------------
-- Transaction fee calculation
--

runTxCalculateMinFeeCmd :: ()
  => TxBodyFile In
  -> NetworkId
  -> ProtocolParamsFile
  -> TxInCount
  -> TxOutCount
  -> TxShelleyWitnessCount
  -> TxByronWitnessCount
  -> ExceptT TxCmdError IO ()
runTxCalculateMinFeeCmd (File txbodyFilePath) nw pParamsFile
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
                             (protocolParamTxFeeFixed pparams)
                             (protocolParamTxFeePerByte pparams)
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
                             (protocolParamTxFeeFixed pparams)
                             (protocolParamTxFeePerByte pparams)
                             tx
                             nInputs nOutputs
                             nByronKeyWitnesses nShelleyKeyWitnesses

        liftIO $ putStrLn $ (show fee :: String) <> " Lovelace"

-- ----------------------------------------------------------------------------
-- Transaction fee calculation
--

runTxCalculateMinRequiredUTxOCmd :: ()
  => CardanoEra era
  -> ProtocolParamsFile
  -> TxOutAnyEra
  -> ExceptT TxCmdError IO ()
runTxCalculateMinRequiredUTxOCmd era pParamsFile txOut = do
  pp <- firstExceptT TxCmdProtocolParamsError (readProtocolParameters pParamsFile)
  out <- toTxOutInAnyEra era txOut
  case cardanoEraStyle era of
    LegacyByronEra -> error "runTxCalculateMinRequiredUTxOCmd: Byron era not implemented yet"
    ShelleyBasedEra sbe -> do
      firstExceptT TxCmdPParamsErr . hoistEither
        $ checkProtocolParameters sbe pp
      pp' <- hoistEither . first TxCmdProtocolParamsConverstionError $ toLedgerPParams sbe pp
      let minValue = calculateMinimumUTxO sbe out pp'
      liftIO . IO.print $ minValue

runTxCreatePolicyIdCmd :: ScriptFile -> ExceptT TxCmdError IO ()
runTxCreatePolicyIdCmd (ScriptFile sFile) = do
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
  Right $ makeShelleyBootstrapWitness (WitnessNetworkId nw) txBody skey
mkShelleyBootstrapWitness _ txBody (ShelleyBootstrapWitnessSigningKeyData skey (Just addr)) =
  Right $ makeShelleyBootstrapWitness (WitnessByronAddress addr) txBody skey

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

runTxHashScriptDataCmd :: ()
  => ScriptDataOrFile
  -> ExceptT TxCmdError IO ()
runTxHashScriptDataCmd scriptDataOrFile = do
    d <- firstExceptT TxCmdScriptDataError $ readScriptDataOrFile scriptDataOrFile
    liftIO $ BS.putStrLn $ serialiseToRawBytesHex (hashScriptDataBytes d)

runTxGetTxIdCmd :: ()
  => InputTxBodyOrTxFile
  -> ExceptT TxCmdError IO ()
runTxGetTxIdCmd txfile = do
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

runTxViewCmd :: ()
  => TxViewOutputFormat
  -> Maybe (File () Out)
  -> InputTxBodyOrTxFile
  -> ExceptT TxCmdError IO ()
runTxViewCmd yamlOrJson mOutFile = \case
  InputTxBodyFile (File txbodyFilePath) -> do
    txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
    unwitnessed <- firstExceptT TxCmdCddlError . newExceptT
                     $ readFileTxBody txbodyFile
    InAnyCardanoEra era txbody <-
      case unwitnessed of
        UnwitnessedCliFormattedTxBody anyTxBody -> pure anyTxBody
        IncompleteCddlFormattedTx (InAnyCardanoEra era tx) ->
          pure $ InAnyCardanoEra era (getTxBody tx)
    -- Why are we differentiating between a transaction body and a transaction?
    -- In the case of a transaction body, we /could/ simply call @makeSignedTransaction []@
    -- to get a transaction which would allow us to reuse friendlyTxBS. However,
    -- this would mean that we'd have an empty list of witnesses mentioned in the output, which
    -- is arguably not part of the transaction body.
    firstExceptT TxCmdWriteFileError . newExceptT $
      case yamlOrJson of
        TxViewOutputFormatYaml -> friendlyTxBodyYaml mOutFile era txbody
        TxViewOutputFormatJson -> friendlyTxBodyJson mOutFile era txbody
  InputTxFile (File txFilePath) -> do
    txFile <- liftIO $ fileOrPipe txFilePath
    InAnyCardanoEra era tx <- lift (readFileTx txFile) & onLeft (left . TxCmdCddlError)
    firstExceptT TxCmdWriteFileError . newExceptT $
      case yamlOrJson of
        TxViewOutputFormatYaml -> friendlyTxYaml mOutFile era tx
        TxViewOutputFormatJson -> friendlyTxJson mOutFile era tx


-- ----------------------------------------------------------------------------
-- Witness commands
--

runTxCreateWitnessCmd :: ()
  => TxBodyFile In
  -> WitnessSigningData
  -> Maybe NetworkId
  -> File () Out
  -> ExceptT TxCmdError IO ()
runTxCreateWitnessCmd (File txbodyFilePath) witSignData mbNw oFile = do
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
       case categoriseSomeSigningWitness someWit of
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
        case categoriseSomeSigningWitness someWit of
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

runTxSignWitnessCmd :: ()
  => TxBodyFile In
  -> [WitnessFile]
  -> File () Out
  -> ExceptT TxCmdError IO ()
runTxSignWitnessCmd (File txbodyFilePath) witnessFiles oFp = do
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
                   Nothing   -> left $ TxCmdWitnessEraMismatch
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
                   Nothing   -> left $ TxCmdWitnessEraMismatch
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
                       -> ExceptT TxCmdError IO
                                  (InAnyShelleyBasedEra a)
onlyInShelleyBasedEras notImplMsg (InAnyCardanoEra era x) =
    case cardanoEraStyle era of
      LegacyByronEra      -> left (TxCmdNotImplemented notImplMsg)
      ShelleyBasedEra sbe -> return (InAnyShelleyBasedEra sbe x)
