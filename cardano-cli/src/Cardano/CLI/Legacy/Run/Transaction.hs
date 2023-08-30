{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- HLINT ignore "Unused LANGUAGE pragma" -}
{- HLINT ignore "Use let" -}

module Cardano.CLI.Legacy.Run.Transaction
  ( runLegacyTransactionCmds
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Json.Friendly (friendlyTxBS, friendlyTxBodyBS)
import           Cardano.CLI.Legacy.Commands.Transaction
import           Cardano.CLI.Legacy.Run.Genesis
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.ShelleyTxCmdError
import           Cardano.CLI.Types.Errors.TxValidationError
import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Output
import           Cardano.CLI.Types.Transaction
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
import           Data.Function ((&))
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Type.Equality (TestEquality (..))
import qualified System.IO as IO

runLegacyTransactionCmds :: LegacyTransactionCmds -> ExceptT ShelleyTxCmdError IO ()
runLegacyTransactionCmds cmd =
  case cmd of
    TxBuild mNodeSocketPath era consensusModeParams nid mScriptValidity mOverrideWits txins readOnlyRefIns
            reqSigners txinsc mReturnColl mTotCollateral txouts changeAddr mValue mLowBound
            mUpperBound certs wdrls metadataSchema scriptFiles metadataFiles mUpProp mconwayVote
            mNewConstitution outputOptions -> do
      runLegacyTxBuildCmd mNodeSocketPath era consensusModeParams nid mScriptValidity mOverrideWits txins readOnlyRefIns
            reqSigners txinsc mReturnColl mTotCollateral txouts changeAddr mValue mLowBound
            mUpperBound certs wdrls metadataSchema scriptFiles metadataFiles mUpProp mconwayVote
            mNewConstitution outputOptions
    TxBuildRaw era mScriptValidity txins readOnlyRefIns txinsc mReturnColl
               mTotColl reqSigners txouts mValue mLowBound mUpperBound fee certs wdrls
               metadataSchema scriptFiles metadataFiles mProtocolParamsFile mUpProp out -> do
      runLegacyTxBuildRawCmd era mScriptValidity txins readOnlyRefIns txinsc mReturnColl
               mTotColl reqSigners txouts mValue mLowBound mUpperBound fee certs wdrls
               metadataSchema scriptFiles metadataFiles mProtocolParamsFile mUpProp out
    TxSign txinfile skfiles network txoutfile ->
      runLegacyTxSignCmd txinfile skfiles network txoutfile
    TxSubmit mNodeSocketPath anyConsensusModeParams network txFp ->
      runLegacyTxSubmitCmd mNodeSocketPath anyConsensusModeParams network txFp
    TxCalculateMinFee txbody nw pParamsFile nInputs nOutputs nShelleyKeyWitnesses nByronKeyWitnesses ->
      runLegacyTxCalculateMinFeeCmd txbody nw pParamsFile nInputs nOutputs nShelleyKeyWitnesses nByronKeyWitnesses
    TxCalculateMinRequiredUTxO era pParamsFile txOuts ->
      runLegacyTxCalculateMinRequiredUTxOCmd era pParamsFile txOuts
    TxHashScriptData scriptDataOrFile ->
      runLegacyTxHashScriptDataCmd scriptDataOrFile
    TxGetTxId txinfile ->
      runLegacyTxGetTxIdCmd txinfile
    TxView txinfile ->
      runLegacyTxViewCmd txinfile
    TxMintedPolicyId sFile ->
      runLegacyTxCreatePolicyIdCmd sFile
    TxCreateWitness txBodyfile witSignData mbNw outFile ->
      runLegacyTxCreateWitnessCmd txBodyfile witSignData mbNw outFile
    TxAssembleTxBodyWitness txBodyFile witnessFile outFile ->
      runLegacyTxSignWitnessCmd txBodyFile witnessFile outFile

-- ----------------------------------------------------------------------------
-- Building transactions
--

runLegacyTxBuildCmd
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
  -> [ProposalFile In]
  -> TxBuildOutputOptions
  -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxBuildCmd
    socketPath (AnyCardanoEra cEra) consensusModeParams@(AnyConsensusModeParams cModeParams) nid
    mScriptValidity mOverrideWits txins readOnlyRefIns reqSigners txinsc mReturnColl mTotCollateral txouts
    changeAddr mValue mLowBound mUpperBound certs wdrls metadataSchema scriptFiles metadataFiles mUpProp
    conwayVotes newProposals outputOptions = do

  -- The user can specify an era prior to the era that the node is currently in.
  -- We cannot use the user specified era to construct a query against a node because it may differ
  -- from the node's era and this will result in the 'QueryEraMismatch' failure.

  let localNodeConnInfo = LocalNodeConnectInfo
                            { localConsensusModeParams = cModeParams
                            , localNodeNetworkId = nid
                            , localNodeSocketPath = socketPath
                            }

  inputsAndMaybeScriptWits <- firstExceptT ShelleyTxCmdScriptWitnessError $ readScriptWitnessFiles cEra txins
  certFilesAndMaybeScriptWits <- firstExceptT ShelleyTxCmdScriptWitnessError $ readScriptWitnessFiles cEra certs

  -- TODO: Conway Era - How can we make this more composable?
  certsAndMaybeScriptWits <-
    case cardanoEraStyle cEra of
      LegacyByronEra -> return []
      ShelleyBasedEra{} ->
        sequence
          [ fmap (,mSwit) (firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT $
              readFileTextEnvelope AsCertificate (File certFile))
          | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
          ]
  withdrawalsAndMaybeScriptWits <- firstExceptT ShelleyTxCmdScriptWitnessError
                                     $ readScriptWitnessFilesThruple cEra wdrls
  txMetadata <- firstExceptT ShelleyTxCmdMetadataError
                  . newExceptT $ readTxMetadata cEra metadataSchema metadataFiles
  valuesWithScriptWits <- readValueScriptWitnesses cEra $ fromMaybe (mempty, []) mValue
  scripts <- firstExceptT ShelleyTxCmdScriptFileError $
                     mapM (readFileScriptInAnyLang . unScriptFile) scriptFiles
  txAuxScripts <- hoistEither $ first ShelleyTxCmdAuxScriptsValidationError $ validateTxAuxScripts cEra scripts

  mProp <- forM mUpProp $ \(UpdateProposalFile upFp) ->
    firstExceptT ShelleyTxCmdReadTextViewFileError (newExceptT $ readFileTextEnvelope AsUpdateProposal (File upFp))
  requiredSigners  <- mapM (firstExceptT ShelleyTxCmdRequiredSignerError .  newExceptT . readRequiredSigner) reqSigners
  mReturnCollateral <- forM mReturnColl $ toTxOutInAnyEra cEra

  txOuts <- mapM (toTxOutInAnyEra cEra) txouts

  -- Conway related
  votes <-
    featureInEra
      (pure emptyVotingProcedures)
      (\w -> firstExceptT ShelleyTxCmdVoteError $ ExceptT (readVotingProceduresFiles w conwayVotes))
      cEra

  proposals <- newExceptT $ first ShelleyTxCmdConstitutionError
                  <$> readTxGovernanceActions cEra newProposals

  -- the same collateral input can be used for several plutus scripts
  let filteredTxinsc = Set.toList $ Set.fromList txinsc

  -- We need to construct the txBodycontent outside of runTxBuild
  BalancedTxBody txBodycontent balancedTxBody _ _ <-
    runTxBuild
      cEra socketPath consensusModeParams nid mScriptValidity inputsAndMaybeScriptWits readOnlyRefIns filteredTxinsc
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

      pparams <- pure mTxProtocolParams & onNothing (left ShelleyTxCmdProtocolParametersNotPresentInTxBody)
      pp <- case cardanoEraStyle cEra of
              LegacyByronEra -> left ShelleyTxCmdByronEra
              ShelleyBasedEra sbe ->
                hoistEither . first ShelleyTxCmdProtocolParamsConverstionError $ toLedgerPParams sbe pparams

      executionUnitPrices <- pure (protocolParamPrices pparams) & onNothing (left ShelleyTxCmdPParamExecutionUnitsNotAvailable)

      let consensusMode = consensusModeOnly cModeParams

      case consensusMode of
        CardanoMode -> do
          AnyCardanoEra nodeEra <- lift (executeLocalStateQueryExpr localNodeConnInfo Nothing (determineEraExpr cModeParams))
            & onLeft (left . ShelleyTxCmdQueryConvenienceError . AcqFailure)
            & onLeft (left . ShelleyTxCmdQueryConvenienceError . QceUnsupportedNtcVersion)

          (nodeEraUTxO, _, eraHistory, systemStart, _, _) <-
            lift (executeLocalStateQueryExpr localNodeConnInfo Nothing (queryStateForBalancedTx nodeEra allTxInputs []))
              & onLeft (left . ShelleyTxCmdQueryConvenienceError . AcqFailure)
              & onLeft (left . ShelleyTxCmdQueryConvenienceError)

          -- Why do we cast the era? The user can specify an era prior to the era that the node is currently in.
          -- We cannot use the user specified era to construct a query against a node because it may differ
          -- from the node's era and this will result in the 'QueryEraMismatch' failure.
          txEraUtxo <- pure (eraCast cEra nodeEraUTxO) & onLeft (left . ShelleyTxCmdTxEraCastErr)

          scriptExecUnitsMap <-
            firstExceptT ShelleyTxCmdTxExecUnitsErr $ hoistEither
              $ evaluateTransactionExecutionUnits
                  systemStart (toLedgerEpochInfo eraHistory)
                  pp txEraUtxo balancedTxBody

          scriptCostOutput <-
            firstExceptT ShelleyTxCmdPlutusScriptCostErr $ hoistEither
              $ renderScriptCosts
                  txEraUtxo
                  executionUnitPrices
                  mScriptWits
                  scriptExecUnitsMap
          liftIO $ LBS.writeFile (unFile fp) $ encodePretty scriptCostOutput
        _ -> left ShelleyTxCmdPlutusScriptsRequireCardanoMode

    OutputTxBodyOnly fpath ->
      let noWitTx = makeSignedTransaction [] balancedTxBody
      in  lift (writeTxFileTextEnvelopeCddl fpath noWitTx)
            & onLeft (left . ShelleyTxCmdWriteFileError)


runLegacyTxBuildRawCmd
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
  -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxBuildRawCmd
  (AnyCardanoEra cEra) mScriptValidity txins readOnlyRefIns txinsc mReturnColl
  mTotColl reqSigners txouts mValue mLowBound mUpperBound fee certs wdrls
  metadataSchema scriptFiles metadataFiles mpParamsFile mUpProp out = do
  inputsAndMaybeScriptWits <- firstExceptT ShelleyTxCmdScriptWitnessError
                                $ readScriptWitnessFiles cEra txins
  certFilesAndMaybeScriptWits <- firstExceptT ShelleyTxCmdScriptWitnessError
                                   $ readScriptWitnessFiles cEra certs

  -- TODO: Conway era - How can we make this more composable?
  certsAndMaybeScriptWits <-
      case cardanoEraStyle cEra of
        LegacyByronEra -> return []
        ShelleyBasedEra{} ->
          sequence
            [ fmap (,mSwit) (firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT $
                readFileTextEnvelope AsCertificate (File certFile))
            | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
            ]

  withdrawalsAndMaybeScriptWits <- firstExceptT ShelleyTxCmdScriptWitnessError
                                     $ readScriptWitnessFilesThruple cEra wdrls
  txMetadata <- firstExceptT ShelleyTxCmdMetadataError
                  . newExceptT $ readTxMetadata cEra metadataSchema metadataFiles
  valuesWithScriptWits <- readValueScriptWitnesses cEra $ fromMaybe (mempty, []) mValue
  scripts <- firstExceptT ShelleyTxCmdScriptFileError $
                     mapM (readFileScriptInAnyLang . unScriptFile) scriptFiles
  txAuxScripts <- hoistEither $ first ShelleyTxCmdAuxScriptsValidationError $ validateTxAuxScripts cEra scripts

  pparams <- forM mpParamsFile $ \ppf ->
    firstExceptT ShelleyTxCmdProtocolParamsError (readProtocolParameters ppf)

  mProp <- forM mUpProp $ \(UpdateProposalFile upFp) ->
    firstExceptT ShelleyTxCmdReadTextViewFileError (newExceptT $ readFileTextEnvelope AsUpdateProposal (File upFp))

  requiredSigners  <- mapM (firstExceptT ShelleyTxCmdRequiredSignerError .  newExceptT . readRequiredSigner) reqSigners
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
    & onLeft (left . ShelleyTxCmdWriteFileError)

-- ----------------------------------------------------------------------------
-- Transaction signing
--

runLegacyTxSignCmd :: InputTxBodyOrTxFile
          -> [WitnessSigningData]
          -> Maybe NetworkId
          -> TxFile Out
          -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxSignCmd txOrTxBody witSigningData mnw outTxFile = do
  sks <-  mapM (firstExceptT ShelleyTxCmdReadWitnessSigningDataError . newExceptT . readWitnessSigningData) witSigningData

  let (sksByron, sksShelley) = partitionSomeWitnesses $ map categoriseSomeWitness sks

  case txOrTxBody of
    InputTxFile (File inputTxFilePath) -> do
      inputTxFile <- liftIO $ fileOrPipe inputTxFilePath
      anyTx <- lift (readFileTx inputTxFile) & onLeft (left . ShelleyTxCmdCddlError)

      InAnyShelleyBasedEra _era tx <-
          onlyInShelleyBasedEras "sign for Byron era transactions" anyTx

      let (txbody, existingTxKeyWits) = getTxBodyAndWitnesses tx

      byronWitnesses <- firstExceptT ShelleyTxCmdBootstrapWitnessError
                          . hoistEither
                          $ mkShelleyBootstrapWitnesses mnw txbody sksByron

      let newShelleyKeyWits = map (makeShelleyKeyWitness txbody) sksShelley
          allKeyWits = existingTxKeyWits ++ newShelleyKeyWits ++ byronWitnesses
          signedTx = makeSignedTransaction allKeyWits txbody

      lift (writeTxFileTextEnvelopeCddl outTxFile signedTx)
        & onLeft (left . ShelleyTxCmdWriteFileError)

    InputTxBodyFile (File txbodyFilePath) -> do
      txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
      unwitnessed <- firstExceptT ShelleyTxCmdCddlError . newExceptT
                       $ readFileTxBody txbodyFile

      case unwitnessed of
        IncompleteCddlFormattedTx anyTx -> do
         InAnyShelleyBasedEra _era unwitTx <-
           onlyInShelleyBasedEras "sign for Byron era transactions" anyTx

         let txbody = getTxBody unwitTx
         -- Byron witnesses require the network ID. This can either be provided
         -- directly or derived from a provided Byron address.
         byronWitnesses <- firstExceptT ShelleyTxCmdBootstrapWitnessError
           . hoistEither
           $ mkShelleyBootstrapWitnesses mnw txbody sksByron

         let shelleyKeyWitnesses = map (makeShelleyKeyWitness txbody) sksShelley
             tx = makeSignedTransaction (byronWitnesses ++ shelleyKeyWitnesses) txbody

         lift (writeTxFileTextEnvelopeCddl outTxFile tx)
            & onLeft (left . ShelleyTxCmdWriteFileError)

        UnwitnessedCliFormattedTxBody anyTxbody -> do
          InAnyShelleyBasedEra _era txbody <-
            --TODO: in principle we should be able to support Byron era txs too
            onlyInShelleyBasedEras "sign for Byron era transactions" anyTxbody
          -- Byron witnesses require the network ID. This can either be provided
          -- directly or derived from a provided Byron address.
          byronWitnesses <- firstExceptT ShelleyTxCmdBootstrapWitnessError
            . hoistEither
            $ mkShelleyBootstrapWitnesses mnw txbody sksByron

          let shelleyKeyWitnesses = map (makeShelleyKeyWitness txbody) sksShelley
              tx = makeSignedTransaction (byronWitnesses ++ shelleyKeyWitnesses) txbody

          firstExceptT ShelleyTxCmdWriteFileError . newExceptT
            $ writeLazyByteStringFile outTxFile
            $ textEnvelopeToJSON Nothing tx

-- ----------------------------------------------------------------------------
-- Transaction submission
--


runLegacyTxSubmitCmd
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> FilePath
  -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxSubmitCmd socketPath (AnyConsensusModeParams cModeParams) network txFilePath = do
    txFile <- liftIO $ fileOrPipe txFilePath
    InAnyCardanoEra era tx <- lift (readFileTx txFile) & onLeft (left . ShelleyTxCmdCddlError)
    let cMode = AnyConsensusMode $ consensusModeOnly cModeParams
    eraInMode <- hoistMaybe
                   (ShelleyTxCmdEraConsensusModeMismatch (Just txFilePath) cMode (AnyCardanoEra era))
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
          TxValidationErrorInMode err _eraInMode -> left . ShelleyTxCmdTxSubmitError . Text.pack $ show err
          TxValidationEraMismatch mismatchErr -> left $ ShelleyTxCmdTxSubmitErrorEraMismatch mismatchErr

-- ----------------------------------------------------------------------------
-- Transaction fee calculation
--

runLegacyTxCalculateMinFeeCmd
  :: TxBodyFile In
  -> NetworkId
  -> ProtocolParamsFile
  -> TxInCount
  -> TxOutCount
  -> TxShelleyWitnessCount
  -> TxByronWitnessCount
  -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxCalculateMinFeeCmd (File txbodyFilePath) nw pParamsFile
                     (TxInCount nInputs) (TxOutCount nOutputs)
                     (TxShelleyWitnessCount nShelleyKeyWitnesses)
                     (TxByronWitnessCount nByronKeyWitnesses) = do

    txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
    unwitnessed <- firstExceptT ShelleyTxCmdCddlError . newExceptT
                     $ readFileTxBody txbodyFile
    pparams <- firstExceptT ShelleyTxCmdProtocolParamsError $ readProtocolParameters pParamsFile
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

runLegacyTxCalculateMinRequiredUTxOCmd
  :: AnyCardanoEra
  -> ProtocolParamsFile
  -> TxOutAnyEra
  -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxCalculateMinRequiredUTxOCmd (AnyCardanoEra era) pParamsFile txOut = do
  pp <- firstExceptT ShelleyTxCmdProtocolParamsError (readProtocolParameters pParamsFile)
  out <- toTxOutInAnyEra era txOut
  case cardanoEraStyle era of
    LegacyByronEra -> error "runLegacyTxCalculateMinRequiredUTxOCmd: Byron era not implemented yet"
    ShelleyBasedEra sbe -> do
      firstExceptT ShelleyTxCmdPParamsErr . hoistEither
        $ checkProtocolParameters sbe pp
      pp' <- hoistEither . first ShelleyTxCmdProtocolParamsConverstionError $ toLedgerPParams sbe pp
      let minValue = calculateMinimumUTxO sbe out pp'
      liftIO . IO.print $ minValue

runLegacyTxCreatePolicyIdCmd :: ScriptFile -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxCreatePolicyIdCmd (ScriptFile sFile) = do
  ScriptInAnyLang _ script <- firstExceptT ShelleyTxCmdScriptFileError $
                                readFileScriptInAnyLang sFile
  liftIO . Text.putStrLn . serialiseToRawBytesHexText $ hashScript script

-- ----------------------------------------------------------------------------
-- Other misc small commands
--

runLegacyTxHashScriptDataCmd :: ScriptDataOrFile -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxHashScriptDataCmd scriptDataOrFile = do
    d <- firstExceptT ShelleyTxCmdScriptDataError $ readScriptDataOrFile scriptDataOrFile
    liftIO $ BS.putStrLn $ serialiseToRawBytesHex (hashScriptDataBytes d)

runLegacyTxGetTxIdCmd :: InputTxBodyOrTxFile -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxGetTxIdCmd txfile = do
    InAnyCardanoEra _era txbody <-
      case txfile of
        InputTxBodyFile (File txbodyFilePath) -> do
          txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
          unwitnessed <- firstExceptT ShelleyTxCmdCddlError . newExceptT
                           $ readFileTxBody txbodyFile
          case unwitnessed of
            UnwitnessedCliFormattedTxBody anyTxBody -> return anyTxBody
            IncompleteCddlFormattedTx (InAnyCardanoEra era tx) ->
              return (InAnyCardanoEra era (getTxBody tx))

        InputTxFile (File txFilePath) -> do
          txFile <- liftIO $ fileOrPipe txFilePath
          InAnyCardanoEra era tx <- lift (readFileTx txFile) & onLeft (left . ShelleyTxCmdCddlError)
          return . InAnyCardanoEra era $ getTxBody tx

    liftIO $ BS.putStrLn $ serialiseToRawBytesHex (getTxId txbody)

runLegacyTxViewCmd :: InputTxBodyOrTxFile -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxViewCmd = \case
  InputTxBodyFile (File txbodyFilePath) -> do
    txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
    unwitnessed <- firstExceptT ShelleyTxCmdCddlError . newExceptT
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
    InAnyCardanoEra era tx <- lift (readFileTx txFile) & onLeft (left . ShelleyTxCmdCddlError)
    liftIO $ BS.putStr $ friendlyTxBS era tx


-- ----------------------------------------------------------------------------
-- Witness commands
--

runLegacyTxCreateWitnessCmd
  :: TxBodyFile In
  -> WitnessSigningData
  -> Maybe NetworkId
  -> File () Out
  -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxCreateWitnessCmd (File txbodyFilePath) witSignData mbNw oFile = do
  txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
  unwitnessed <- firstExceptT ShelleyTxCmdCddlError . newExceptT
                   $ readFileTxBody txbodyFile
  case unwitnessed of
    IncompleteCddlFormattedTx anyTx -> do
     InAnyShelleyBasedEra sbe cddlTx <-
       onlyInShelleyBasedEras "sign for Byron era transactions" anyTx

     let txbody = getTxBody cddlTx
     someWit <- firstExceptT ShelleyTxCmdReadWitnessSigningDataError
                  . newExceptT $ readWitnessSigningData witSignData
     witness <-
       case categoriseSomeWitness someWit of
         -- Byron witnesses require the network ID. This can either be provided
         -- directly or derived from a provided Byron address.
         AByronWitness bootstrapWitData ->
           firstExceptT ShelleyTxCmdBootstrapWitnessError
             . hoistEither
             $ mkShelleyBootstrapWitness mbNw txbody bootstrapWitData
         AShelleyKeyWitness skShelley ->
           pure $ makeShelleyKeyWitness txbody skShelley

     firstExceptT ShelleyTxCmdWriteFileError . newExceptT
       $ writeTxWitnessFileTextEnvelopeCddl sbe oFile witness

    UnwitnessedCliFormattedTxBody anyTxbody -> do
      InAnyShelleyBasedEra _era txbody <-
        onlyInShelleyBasedEras "sign for Byron era transactions" anyTxbody

      someWit <- firstExceptT ShelleyTxCmdReadWitnessSigningDataError
                   . newExceptT $ readWitnessSigningData witSignData

      witness <-
        case categoriseSomeWitness someWit of
          -- Byron witnesses require the network ID. This can either be provided
          -- directly or derived from a provided Byron address.
          AByronWitness bootstrapWitData ->
            firstExceptT ShelleyTxCmdBootstrapWitnessError
              . hoistEither
              $ mkShelleyBootstrapWitness mbNw txbody bootstrapWitData
          AShelleyKeyWitness skShelley ->
            pure $ makeShelleyKeyWitness txbody skShelley

      firstExceptT ShelleyTxCmdWriteFileError . newExceptT
        $ writeLazyByteStringFile oFile
        $ textEnvelopeToJSON Nothing witness

runLegacyTxSignWitnessCmd
  :: TxBodyFile In
  -> [WitnessFile]
  -> File () Out
  -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxSignWitnessCmd (File txbodyFilePath) witnessFiles oFp = do
    txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
    unwitnessed <- firstExceptT ShelleyTxCmdCddlError . newExceptT
                     $ readFileTxBody txbodyFile
    case unwitnessed of
      UnwitnessedCliFormattedTxBody (InAnyCardanoEra era txbody) -> do
        witnesses <-
          sequence
            [ do InAnyCardanoEra era' witness <- firstExceptT ShelleyTxCmdCddlWitnessError . newExceptT
                                                   $ readFileTxKeyWitness file
                 case testEquality era era' of
                   Nothing   -> left $ ShelleyTxCmdWitnessEraMismatch
                                         (AnyCardanoEra era)
                                         (AnyCardanoEra era')
                                         witnessFile
                   Just Refl -> return witness
            | witnessFile@(WitnessFile file) <- witnessFiles
            ]

        let tx = makeSignedTransaction witnesses txbody
        firstExceptT ShelleyTxCmdWriteFileError
          . newExceptT
          $ writeLazyByteStringFile oFp
          $ textEnvelopeToJSON Nothing tx

      IncompleteCddlFormattedTx (InAnyCardanoEra era anyTx) -> do
        let txbody = getTxBody anyTx

        witnesses <-
          sequence
            [ do InAnyCardanoEra era' witness <- firstExceptT ShelleyTxCmdCddlWitnessError . newExceptT
                                                   $ readFileTxKeyWitness file
                 case testEquality era era' of
                   Nothing   -> left $ ShelleyTxCmdWitnessEraMismatch
                                         (AnyCardanoEra era)
                                         (AnyCardanoEra era')
                                         witnessFile
                   Just Refl -> return witness
            | witnessFile@(WitnessFile file) <- witnessFiles ]

        let tx = makeSignedTransaction witnesses txbody

        lift (writeTxFileTextEnvelopeCddl oFp tx) & onLeft (left . ShelleyTxCmdWriteFileError)
