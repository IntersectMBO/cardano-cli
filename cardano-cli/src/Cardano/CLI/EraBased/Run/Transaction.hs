{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- HLINT ignore "Unused LANGUAGE pragma" -}
{- HLINT ignore "Avoid lambda using `infix`" -}

module Cardano.CLI.EraBased.Run.Transaction
  ( runTransactionCmds
  , runTransactionBuildCmd
  , runTransactionBuildRawCmd
  , runTransactionSignCmd
  , runTransactionSubmitCmd
  , runTransactionCalculateMinFeeCmd
  , runTransactionCalculateMinValueCmd
  , runTransactionPolicyIdCmd
  , runTransactionHashScriptDataCmd
  , runTransactionTxIdCmd
  , runTransactionViewCmd
  , runTransactionWitnessCmd
  , runTransactionSignWitnessCmd
  , toTxOutByronEra
  ) where

import           Cardano.Api
import           Cardano.Api.Byron hiding (SomeByronSigningKey (..))
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import qualified Cardano.CLI.EraBased.Commands.Transaction as Cmd
import           Cardano.CLI.EraBased.Run.Genesis
import           Cardano.CLI.Json.Friendly (FriendlyFormat (..), friendlyTx, friendlyTxBody)
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.BootstrapWitnessError
import           Cardano.CLI.Types.Errors.NodeEraMismatchError
import           Cardano.CLI.Types.Errors.TxCmdError
import           Cardano.CLI.Types.Errors.TxValidationError
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
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Type.Equality (TestEquality (..))
import           Lens.Micro ((^.))
import qualified System.IO as IO


runTransactionCmds :: Cmd.TransactionCmds era -> ExceptT TxCmdError IO ()
runTransactionCmds = \case
  Cmd.TransactionBuildCmd             args -> runTransactionBuildCmd args
  Cmd.TransactionBuildRawCmd          args -> runTransactionBuildRawCmd args
  Cmd.TransactionSignCmd              args -> runTransactionSignCmd args
  Cmd.TransactionSubmitCmd            args -> runTransactionSubmitCmd args
  Cmd.TransactionCalculateMinFeeCmd   args -> runTransactionCalculateMinFeeCmd args
  Cmd.TransactionCalculateMinValueCmd args -> runTransactionCalculateMinValueCmd args
  Cmd.TransactionHashScriptDataCmd    args -> runTransactionHashScriptDataCmd args
  Cmd.TransactionTxIdCmd              args -> runTransactionTxIdCmd args
  Cmd.TransactionViewCmd              args -> runTransactionViewCmd args
  Cmd.TransactionPolicyIdCmd          args -> runTransactionPolicyIdCmd args
  Cmd.TransactionWitnessCmd           args -> runTransactionWitnessCmd args
  Cmd.TransactionSignWitnessCmd       args -> runTransactionSignWitnessCmd args

-- ----------------------------------------------------------------------------
-- Building transactions
--

runTransactionBuildCmd :: ()
  => Cmd.TransactionBuildCmdArgs era
  -> ExceptT TxCmdError IO ()
runTransactionBuildCmd
    Cmd.TransactionBuildCmdArgs
      { eon
      , nodeSocketPath
      , consensusModeParams
      , networkId = networkId
      , mScriptValidity = mScriptValidity
      , mOverrideWitnesses = mOverrideWitnesses
      , txins
      , readOnlyReferenceInputs
      , requiredSigners = reqSigners
      , txinsc
      , mReturnCollateral = mReturnColl
      , mTotalCollateral
      , txouts
      , changeAddresses
      , mValue
      , mValidityLowerBound
      , mValidityUpperBound
      , certificates
      , withdrawals
      , metadataSchema
      , scriptFiles
      , metadataFiles
      , mfUpdateProposalFile
      , voteFiles
      , proposalFiles
      , buildOutputOptions
      } = shelleyBasedEraConstraints eon $ do
  let era = shelleyBasedToCardanoEra eon

  -- The user can specify an era prior to the era that the node is currently in.
  -- We cannot use the user specified era to construct a query against a node because it may differ
  -- from the node's era and this will result in the 'QueryEraMismatch' failure.

  let localNodeConnInfo = LocalNodeConnectInfo
                            { localConsensusModeParams = consensusModeParams
                            , localNodeNetworkId = networkId
                            , localNodeSocketPath = nodeSocketPath
                            }

  inputsAndMaybeScriptWits <- firstExceptT TxCmdScriptWitnessError $ readScriptWitnessFiles eon txins
  certFilesAndMaybeScriptWits <- firstExceptT TxCmdScriptWitnessError $ readScriptWitnessFiles eon certificates

  -- TODO: Conway Era - How can we make this more composable?
  certsAndMaybeScriptWits <-
      sequence
        [ fmap (,mSwit) (firstExceptT TxCmdReadTextViewFileError . newExceptT $
            readFileTextEnvelope AsCertificate (File certFile))
        | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
        ]
  withdrawalsAndMaybeScriptWits <- firstExceptT TxCmdScriptWitnessError $
    readScriptWitnessFilesThruple eon withdrawals
  txMetadata <- firstExceptT TxCmdMetadataError . newExceptT $
    readTxMetadata eon metadataSchema metadataFiles
  valuesWithScriptWits <- readValueScriptWitnesses eon $ fromMaybe mempty mValue
  scripts <- firstExceptT TxCmdScriptFileError $
    mapM (readFileScriptInAnyLang . unScriptFile) scriptFiles
  txAuxScripts <- hoistEither $ first TxCmdAuxScriptsValidationError $ validateTxAuxScripts eon scripts

  mProp <- case mfUpdateProposalFile of
    Just (Featured w (Just updateProposalFile)) ->
      readTxUpdateProposal w updateProposalFile & firstExceptT TxCmdReadTextViewFileError
    _ -> pure TxUpdateProposalNone

  requiredSigners  <- mapM (firstExceptT TxCmdRequiredSignerError .  newExceptT . readRequiredSigner) reqSigners
  mReturnCollateral <- forM mReturnColl $ toTxOutInShelleyBasedEra eon

  txOuts <- mapM (toTxOutInAnyEra eon) txouts

  -- Conway related
  votingProceduresAndMaybeScriptWits <-
    inEonForEra
      (pure mempty)
      (\w -> firstExceptT TxCmdVoteError $ ExceptT (readVotingProceduresFiles w voteFiles))
      era

  proposals <- newExceptT $ first TxCmdProposalError
                  <$> readTxGovernanceActions eon proposalFiles

  -- the same collateral input can be used for several plutus scripts
  let filteredTxinsc = Set.toList $ Set.fromList txinsc

  -- We need to construct the txBodycontent outside of runTxBuild
  BalancedTxBody txBodyContent balancedTxBody _ _ <-
    runTxBuild
      eon nodeSocketPath networkId mScriptValidity inputsAndMaybeScriptWits readOnlyReferenceInputs
      filteredTxinsc mReturnCollateral mTotalCollateral txOuts changeAddresses valuesWithScriptWits
      mValidityLowerBound mValidityUpperBound certsAndMaybeScriptWits withdrawalsAndMaybeScriptWits
      requiredSigners txAuxScripts txMetadata mProp mOverrideWitnesses votingProceduresAndMaybeScriptWits
      proposals buildOutputOptions

  let mScriptWits =
        forEraInEon era [] $ \sbe -> collectTxBodyScriptWitnesses sbe txBodyContent
      allReferenceInputs = getAllReferenceInputs
                             inputsAndMaybeScriptWits
                             (snd valuesWithScriptWits)
                             certsAndMaybeScriptWits
                             withdrawalsAndMaybeScriptWits
                             votingProceduresAndMaybeScriptWits
                             proposals
                             readOnlyReferenceInputs

  let inputsThatRequireWitnessing = [input | (input,_) <- inputsAndMaybeScriptWits]
      allTxInputs = inputsThatRequireWitnessing ++ allReferenceInputs ++ filteredTxinsc

  -- TODO: Calculating the script cost should live as a different command.
  -- Why? Because then we can simply read a txbody and figure out
  -- the script cost vs having to build the tx body each time
  case buildOutputOptions of
    OutputScriptCostOnly fp -> do
      let BuildTxWith mTxProtocolParams = txProtocolParams txBodyContent

      pparams <- pure mTxProtocolParams & onNothing (left TxCmdProtocolParametersNotPresentInTxBody)
      executionUnitPrices <- pure (getExecutionUnitPrices era pparams) & onNothing (left TxCmdPParamExecutionUnitsNotAvailable)

      AnyCardanoEra nodeEra <- lift (executeLocalStateQueryExpr localNodeConnInfo Nothing queryCurrentEra)
        & onLeft (left . TxCmdQueryConvenienceError . AcqFailure)
        & onLeft (left . TxCmdQueryConvenienceError . QceUnsupportedNtcVersion)

      (txEraUtxo, _, eraHistory, systemStart, _, _, _) <-
        lift (executeLocalStateQueryExpr localNodeConnInfo Nothing (queryStateForBalancedTx nodeEra allTxInputs []))
          & onLeft (left . TxCmdQueryConvenienceError . AcqFailure)
          & onLeft (left . TxCmdQueryConvenienceError)

      Refl <- testEquality era nodeEra
        & hoistMaybe (TxCmdTxNodeEraMismatchError $ NodeEraMismatchError era nodeEra)

      scriptExecUnitsMap <-
        firstExceptT (TxCmdTxExecUnitsErr . AnyTxCmdTxExecUnitsErr) $ hoistEither
          $ evaluateTransactionExecutionUnits era
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

    OutputTxBodyOnly fpath ->
      let noWitTx = makeSignedTransaction [] balancedTxBody
      in  lift (cardanoEraConstraints era $ writeTxFileTextEnvelopeCddl eon fpath noWitTx)
            & onLeft (left . TxCmdWriteFileError)

getExecutionUnitPrices :: CardanoEra era -> LedgerProtocolParameters era -> Maybe Ledger.Prices
getExecutionUnitPrices cEra (LedgerProtocolParameters pp) =
  forEraInEonMaybe cEra $ \aeo ->
    alonzoEraOnwardsConstraints aeo $
      pp ^. Ledger.ppPricesL

runTransactionBuildRawCmd :: ()
  => Cmd.TransactionBuildRawCmdArgs era
  -> ExceptT TxCmdError IO ()
runTransactionBuildRawCmd
    Cmd.TransactionBuildRawCmdArgs
      { eon
      , mScriptValidity
      , txIns
      , readOnlyRefIns
      , txInsCollateral
      , mReturnCollateral = mReturnColl
      , mTotalCollateral
      , requiredSigners = reqSigners
      , txouts
      , mValue
      , mValidityLowerBound
      , mValidityUpperBound
      , fee
      , certificates
      , withdrawals
      , metadataSchema
      , scriptFiles
      , metadataFiles
      , mProtocolParamsFile
      , mUpdateProprosalFile
      , voteFiles
      , proposalFiles
      , txBodyOutFile
      } = do
  inputsAndMaybeScriptWits <- firstExceptT TxCmdScriptWitnessError
                                $ readScriptWitnessFiles eon txIns
  certFilesAndMaybeScriptWits <- firstExceptT TxCmdScriptWitnessError
                                   $ readScriptWitnessFiles eon certificates

  withdrawalsAndMaybeScriptWits <- firstExceptT TxCmdScriptWitnessError
                                     $ readScriptWitnessFilesThruple eon withdrawals
  txMetadata <- firstExceptT TxCmdMetadataError
                  . newExceptT $ readTxMetadata eon metadataSchema metadataFiles
  valuesWithScriptWits <- readValueScriptWitnesses eon $ fromMaybe mempty mValue
  scripts <- firstExceptT TxCmdScriptFileError $
                     mapM (readFileScriptInAnyLang . unScriptFile) scriptFiles
  txAuxScripts <- hoistEither $ first TxCmdAuxScriptsValidationError $ validateTxAuxScripts eon scripts

  -- TODO: Conway era - update readProtocolParameters to rely on Ledger.PParams JSON instances
  pparams <- forM mProtocolParamsFile $ \ppf ->
    firstExceptT TxCmdProtocolParamsError (readProtocolParameters ppf)

  mLedgerPParams <- case pparams of
                      Nothing -> return Nothing
                      Just pp -> do
                        ledgerpp <- firstExceptT TxCmdProtocolParamsConverstionError . hoistEither $ convertToLedgerProtocolParameters eon pp
                        return $ Just ledgerpp

  txUpdateProposal <- case mUpdateProprosalFile of
    Just (Featured w (Just updateProposalFile)) ->
      readTxUpdateProposal w updateProposalFile & firstExceptT TxCmdReadTextViewFileError
    _ -> pure TxUpdateProposalNone

  requiredSigners  <- mapM (firstExceptT TxCmdRequiredSignerError .  newExceptT . readRequiredSigner) reqSigners

  mReturnCollateral <- case mReturnColl of
                         Nothing -> return Nothing
                         Just retColl -> do
                          txOut <- toTxOutInShelleyBasedEra eon retColl
                          return $ Just txOut

  txOuts <- mapM (toTxOutInAnyEra eon) txouts

    -- the same collateral input can be used for several plutus scripts
  let filteredTxinsc = Set.toList $ Set.fromList txInsCollateral

  -- Conway related
  votingProceduresAndMaybeScriptWits <-
    inEonForShelleyBasedEra
      (pure mempty)
      (\w -> firstExceptT TxCmdVoteError . ExceptT $ conwayEraOnwardsConstraints w $ readVotingProceduresFiles w voteFiles)
      eon

  proposals <-
    lift (readTxGovernanceActions eon proposalFiles)
      & onLeft (left . TxCmdProposalError)

  certsAndMaybeScriptWits <-
      shelleyBasedEraConstraints eon $
        sequence
          [ fmap (,mSwit) (firstExceptT TxCmdReadTextViewFileError . newExceptT $
              readFileTextEnvelope AsCertificate (File certFile))
          | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
          ]
  txBody <-
    hoistEither $ runTxBuildRaw
      eon mScriptValidity inputsAndMaybeScriptWits readOnlyRefIns filteredTxinsc
      mReturnCollateral mTotalCollateral txOuts mValidityLowerBound mValidityUpperBound fee valuesWithScriptWits
      certsAndMaybeScriptWits withdrawalsAndMaybeScriptWits requiredSigners txAuxScripts
      txMetadata mLedgerPParams txUpdateProposal votingProceduresAndMaybeScriptWits proposals

  let noWitTx = makeSignedTransaction [] txBody
  lift (writeTxFileTextEnvelopeCddl eon txBodyOutFile noWitTx)
    & onLeft (left . TxCmdWriteFileError)


runTxBuildRaw :: ()
  => ShelleyBasedEra era
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
  -> TxValidityUpperBound era
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
  -> TxUpdateProposal era
  -> [(VotingProcedures era, Maybe (ScriptWitness WitCtxStake era))]
  -> [(Proposal era, Maybe (ScriptWitness WitCtxStake era))]
  -> Either TxCmdError (TxBody era)
runTxBuildRaw sbe
              mScriptValidity inputsAndMaybeScriptWits
              readOnlyRefIns txinsc
              mReturnCollateral mTotCollateral txouts
              mLowerBound mUpperBound
              mFee valuesWithScriptWits
              certsAndMaybeSriptWits withdrawals reqSigners
              txAuxScripts txMetadata mpparams txUpdateProposal votingProcedures proposals = do

    let era = toCardanoEra sbe
        allReferenceInputs = getAllReferenceInputs
                               inputsAndMaybeScriptWits
                               (snd valuesWithScriptWits)
                               certsAndMaybeSriptWits
                               withdrawals
                               votingProcedures
                               proposals
                               readOnlyRefIns

        validatedTxIns = validateTxIns inputsAndMaybeScriptWits
    validatedCollateralTxIns <- validateTxInsCollateral era txinsc
    validatedRefInputs <- validateTxInsReference era allReferenceInputs
    validatedTotCollateral
      <- first TxCmdTotalCollateralValidationError $ validateTxTotalCollateral era mTotCollateral
    validatedRetCol
      <- first TxCmdReturnCollateralValidationError $ validateTxReturnCollateral era mReturnCollateral
    validatedFee
      <- first TxCmdTxFeeValidationError $ validateTxFee sbe mFee
    validatedLowerBound
      <- first TxCmdTxValidityLowerBoundValidationError (validateTxValidityLowerBound era mLowerBound)
    validatedReqSigners
      <- first TxCmdRequiredSignersValidationError $ validateRequiredSigners era reqSigners
    validatedPParams
      <- first TxCmdProtocolParametersValidationError $ validateProtocolParameters era mpparams
    validatedTxWtdrwls
      <- first TxCmdTxWithdrawalsValidationError $ validateTxWithdrawals era withdrawals
    validatedTxCerts
      <- first TxCmdTxCertificatesValidationError $ validateTxCertificates era certsAndMaybeSriptWits
    validatedMintValue
      <- createTxMintValue sbe valuesWithScriptWits
    validatedTxScriptValidity
      <- first TxCmdScriptValidityValidationError $ validateTxScriptValidity era mScriptValidity
    let txBodyContent =
          TxBodyContent
            { txIns = validatedTxIns
            , txInsCollateral = validatedCollateralTxIns
            , txInsReference = validatedRefInputs
            , txOuts = txouts
            , txTotalCollateral = validatedTotCollateral
            , txReturnCollateral = validatedRetCol
            , txFee = validatedFee
            , txValidityLowerBound = validatedLowerBound
            , txValidityUpperBound = mUpperBound
            , txMetadata = txMetadata
            , txAuxScripts = txAuxScripts
            , txExtraKeyWits = validatedReqSigners
            , txProtocolParams = validatedPParams
            , txWithdrawals = validatedTxWtdrwls
            , txCertificates = validatedTxCerts
            , txUpdateProposal = txUpdateProposal
            , txMintValue = validatedMintValue
            , txScriptValidity = validatedTxScriptValidity
            , txProposalProcedures = forEraInEonMaybe era (`Featured` (shelleyBasedEraConstraints sbe $ convToTxProposalProcedures proposals))
            , txVotingProcedures = forEraInEonMaybe era (`Featured` convertToTxVotingProcedures votingProcedures)
            }
    first TxCmdTxBodyError $ createAndValidateTransactionBody sbe txBodyContent

runTxBuild :: ()
  => ShelleyBasedEra era
  -> SocketPath
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
  -> TxValidityUpperBound era
  -- ^ Tx upper bound
  -> [(Certificate era, Maybe (ScriptWitness WitCtxStake era))]
  -- ^ Certificate with potential script witness
  -> [(StakeAddress, Lovelace, Maybe (ScriptWitness WitCtxStake era))]
  -> [Hash PaymentKey]
  -- ^ Required signers
  -> TxAuxScripts era
  -> TxMetadataInEra era
  -> TxUpdateProposal era
  -> Maybe Word
  -> [(VotingProcedures era, Maybe (ScriptWitness WitCtxStake era))]
  -> [(Proposal era, Maybe (ScriptWitness WitCtxStake era))]
  -> TxBuildOutputOptions
  -> ExceptT TxCmdError IO (BalancedTxBody era)
runTxBuild
    sbe socketPath networkId mScriptValidity
    inputsAndMaybeScriptWits readOnlyRefIns txinsc mReturnCollateral mTotCollateral txouts
    (TxOutChangeAddress changeAddr) valuesWithScriptWits mLowerBound mUpperBound
    certsAndMaybeScriptWits withdrawals reqSigners txAuxScripts txMetadata
    txUpdateProposal mOverrideWits votingProcedures proposals _outputOptions = shelleyBasedEraConstraints sbe $ do

  -- TODO: All functions should be parameterized by ShelleyBasedEra
  -- as it's not possible to call this function with ByronEra
  let era = shelleyBasedToCardanoEra sbe
      dummyFee = Just $ Lovelace 0
      inputsThatRequireWitnessing = [input | (input,_) <- inputsAndMaybeScriptWits]

  let allReferenceInputs = getAllReferenceInputs
                             inputsAndMaybeScriptWits
                             (snd valuesWithScriptWits)
                             certsAndMaybeScriptWits
                             withdrawals
                             votingProcedures
                             proposals
                             readOnlyRefIns

  validatedCollateralTxIns <- hoistEither $ validateTxInsCollateral era txinsc
  validatedRefInputs <- hoistEither $ validateTxInsReference era allReferenceInputs
  validatedTotCollateral
    <- hoistEither $ first TxCmdTotalCollateralValidationError $ validateTxTotalCollateral era mTotCollateral
  validatedRetCol
    <- hoistEither $ first TxCmdReturnCollateralValidationError $ validateTxReturnCollateral era mReturnCollateral
  dFee <- hoistEither $ first TxCmdTxFeeValidationError $ validateTxFee sbe dummyFee
  validatedLowerBound <- hoistEither (first TxCmdTxValidityLowerBoundValidationError (validateTxValidityLowerBound era mLowerBound))
  validatedReqSigners <- hoistEither (first TxCmdRequiredSignersValidationError $ validateRequiredSigners era reqSigners)
  validatedTxWtdrwls <- hoistEither (first TxCmdTxWithdrawalsValidationError $ validateTxWithdrawals era withdrawals)
  validatedTxCerts <- hoistEither (first TxCmdTxCertificatesValidationError $ validateTxCertificates era certsAndMaybeScriptWits)
  validatedMintValue <- hoistEither $ createTxMintValue sbe valuesWithScriptWits
  validatedTxScriptValidity <- hoistEither (first TxCmdScriptValidityValidationError $ validateTxScriptValidity era mScriptValidity)

  let allTxInputs = inputsThatRequireWitnessing ++ allReferenceInputs ++ txinsc
      localNodeConnInfo = LocalNodeConnectInfo
                                  { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
                                  , localNodeNetworkId = networkId
                                  , localNodeSocketPath = socketPath
                                  }

  AnyCardanoEra nodeEra <- lift (executeLocalStateQueryExpr localNodeConnInfo Nothing queryCurrentEra)
    & onLeft (left . TxCmdQueryConvenienceError . AcqFailure)
    & onLeft (left . TxCmdQueryConvenienceError . QceUnsupportedNtcVersion)

  Refl <- testEquality era nodeEra
    & hoistMaybe (TxCmdTxNodeEraMismatchError $ NodeEraMismatchError era nodeEra)

  let certs =
        case validatedTxCerts of
          TxCertificates _ cs _ -> cs
          _ -> []

  (txEraUtxo, pparams, eraHistory, systemStart, stakePools, stakeDelegDeposits, drepDelegDeposits) <-
    lift (executeLocalStateQueryExpr localNodeConnInfo Consensus.VolatileTip $ queryStateForBalancedTx nodeEra allTxInputs certs)
      & onLeft (left . TxCmdQueryConvenienceError . AcqFailure)
      & onLeft (left . TxCmdQueryConvenienceError)

  validatedPParams <- hoistEither $ first TxCmdProtocolParametersValidationError
                                  $ validateProtocolParameters era (Just pparams)

  let txBodyContent =
        TxBodyContent
          { txIns = validateTxIns inputsAndMaybeScriptWits
          , txInsCollateral = validatedCollateralTxIns
          , txInsReference = validatedRefInputs
          , txOuts = txouts
          , txTotalCollateral = validatedTotCollateral
          , txReturnCollateral = validatedRetCol
          , txFee = dFee
          , txValidityLowerBound = validatedLowerBound
          , txValidityUpperBound = mUpperBound
          , txMetadata = txMetadata
          , txAuxScripts = txAuxScripts
          , txExtraKeyWits = validatedReqSigners
          , txProtocolParams = validatedPParams
          , txWithdrawals = validatedTxWtdrwls
          , txCertificates = validatedTxCerts
          , txUpdateProposal = txUpdateProposal
          , txMintValue = validatedMintValue
          , txScriptValidity = validatedTxScriptValidity
          , txProposalProcedures = forEraInEonMaybe era (`Featured` convToTxProposalProcedures proposals)
          , txVotingProcedures = forEraInEonMaybe era (`Featured` convertToTxVotingProcedures votingProcedures)
          }

  firstExceptT TxCmdTxInsDoNotExist
    . hoistEither $ txInsExistInUTxO allTxInputs txEraUtxo
  firstExceptT TxCmdQueryNotScriptLocked
    . hoistEither $ notScriptLockedTxIns txinsc txEraUtxo

  cAddr <- pure (anyAddressInEra era changeAddr)
    & onLeft (error $ "runTxBuild: Byron address used: " <> show changeAddr) -- should this throw instead?

  balancedTxBody@(BalancedTxBody _ _ _ fee) <-
    firstExceptT (TxCmdBalanceTxBody . AnyTxBodyErrorAutoBalance)
      . hoistEither
      $ makeTransactionBodyAutoBalance sbe systemStart (toLedgerEpochInfo eraHistory)
                                        pparams stakePools stakeDelegDeposits drepDelegDeposits
                                        txEraUtxo txBodyContent cAddr mOverrideWits

  liftIO $ putStrLn $ "Estimated transaction fee: " <> (show fee :: String)

  return balancedTxBody

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
 -> [(VotingProcedures era, Maybe (ScriptWitness WitCtxStake era))]
 -> [(Proposal era, Maybe (ScriptWitness WitCtxStake era))]
 -> [TxIn] -- ^ Read only reference inputs
 -> [TxIn]
getAllReferenceInputs txins mintWitnesses certFiles withdrawals
                      votingProceduresAndMaybeScriptWits propProceduresAnMaybeScriptWits
                      readOnlyRefIns = do
  let txinsWitByRefInputs = [getReferenceInput sWit | (_, Just sWit) <- txins]
      mintingRefInputs = map getReferenceInput mintWitnesses
      certsWitByRefInputs = [getReferenceInput sWit | (_, Just sWit) <- certFiles]
      withdrawalsWitByRefInputs = [getReferenceInput sWit | (_, _, Just sWit) <- withdrawals]
      votesWitByRefInputs = [getReferenceInput sWit | (_, Just sWit) <- votingProceduresAndMaybeScriptWits]
      propsWitByRefInputs = [getReferenceInput sWit | (_, Just sWit) <- propProceduresAnMaybeScriptWits]

  catMaybes $ concat [ txinsWitByRefInputs
                     , mintingRefInputs
                     , certsWitByRefInputs
                     , withdrawalsWitByRefInputs
                     , votesWitByRefInputs
                     , propsWitByRefInputs
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


toAddressInShelleyBasedEra
  :: ShelleyBasedEra era
  -> Address ShelleyAddr
  -> Either TxCmdError (AddressInEra era)
toAddressInShelleyBasedEra sbe sAddr = runExcept $
      pure (AddressInEra (ShelleyAddressInEra sbe) sAddr)


lovelaceToCoin :: Lovelace -> Ledger.Coin
lovelaceToCoin (Lovelace ll) = Ledger.Coin ll

toTxOutValueInAnyEra
  :: ShelleyBasedEra era
  -> Value
  -> Either TxCmdError (TxOutValue era)
toTxOutValueInAnyEra era val =
  caseShelleyToAllegraOrMaryEraOnwards
    (\_ -> case valueToLovelace val of
      Just l  -> return (TxOutValueShelleyBased era $ lovelaceToCoin l)
      Nothing -> txFeatureMismatchPure (toCardanoEra era) TxFeatureMultiAssetOutputs
    )
    (\w -> return (TxOutValueShelleyBased era (toLedgerValue w val))
    )
    era

toTxOutValueInShelleyBasedEra
  :: ShelleyBasedEra era
  -> Value
  -> Either TxCmdError (TxOutValue era)
toTxOutValueInShelleyBasedEra sbe val =
  caseShelleyToAllegraOrMaryEraOnwards
    (\_ -> case valueToLovelace val of
      Just l  -> return (TxOutValueShelleyBased sbe $ lovelaceToCoin l)
      Nothing -> txFeatureMismatchPure (toCardanoEra sbe) TxFeatureMultiAssetOutputs
    )
    (\w -> return (TxOutValueShelleyBased sbe (toLedgerValue w val))
    )
    sbe


toTxOutInShelleyBasedEra
  :: ShelleyBasedEra era
  -> TxOutShelleyBasedEra
  -> ExceptT TxCmdError IO (TxOut CtxTx era)
toTxOutInShelleyBasedEra era (TxOutShelleyBasedEra addr' val' mDatumHash refScriptFp) = do
  addr <- hoistEither $ toAddressInShelleyBasedEra era addr'
  val <- hoistEither $ toTxOutValueInShelleyBasedEra era val'

  datum <-
    caseShelleyToMaryOrAlonzoEraOnwards
      (const (pure TxOutDatumNone))
      (\wa -> toTxAlonzoDatum wa mDatumHash)
      era

  refScript <- inEonForEra
                 (pure ReferenceScriptNone)
                 (\wb -> getReferenceScript wb refScriptFp)
                 (toCardanoEra era)

  pure $ TxOut addr val datum refScript


toTxOutByronEra
  :: TxOutAnyEra
  -> ExceptT TxCmdError IO (TxOut CtxTx ByronEra)
toTxOutByronEra (TxOutAnyEra addr' val' _ _) = do
  addr <- hoistEither $ toAddressInAnyEra ByronEra addr'
  let ada = TxOutValueByron $ selectLovelace val'
  pure $ TxOut addr ada TxOutDatumNone ReferenceScriptNone

-- TODO: toTxOutInAnyEra eventually will not be needed because
-- byron related functionality will be treated
-- separately
toTxOutInAnyEra :: ShelleyBasedEra era
                -> TxOutAnyEra
                -> ExceptT TxCmdError IO (TxOut CtxTx era)
toTxOutInAnyEra era (TxOutAnyEra addr' val' mDatumHash refScriptFp) = do
  let cEra = toCardanoEra era
  addr <- hoistEither $ toAddressInAnyEra cEra addr'
  val <- hoistEither $ toTxOutValueInAnyEra era val'

  datum <-
    caseShelleyToMaryOrAlonzoEraOnwards
      (const (pure TxOutDatumNone))
      (\wa -> toTxAlonzoDatum wa mDatumHash)
      era

  refScript <- caseShelleyToAlonzoOrBabbageEraOnwards
    (const (pure ReferenceScriptNone))
    (\wb -> getReferenceScript wb refScriptFp)
    era
  pure $ TxOut addr val datum refScript

getReferenceScript :: ()
  => BabbageEraOnwards era
  -> ReferenceScriptAnyEra
  -> ExceptT TxCmdError IO (ReferenceScript era)
getReferenceScript w = \case
  ReferenceScriptAnyEraNone -> return ReferenceScriptNone
  ReferenceScriptAnyEra fp -> ReferenceScript w <$> firstExceptT TxCmdScriptFileError (readFileScriptInAnyLang fp)

toTxAlonzoDatum :: ()
  => AlonzoEraOnwards era
  -> TxOutDatumAnyEra
  -> ExceptT TxCmdError IO (TxOutDatum CtxTx era)
toTxAlonzoDatum supp cliDatum =
  case cliDatum of
    TxOutDatumByNone -> pure TxOutDatumNone
    TxOutDatumByHashOnly h -> pure (TxOutDatumHash supp h)
    TxOutDatumByHashOf sDataOrFile -> do
      sData <- firstExceptT TxCmdScriptDataError $ readScriptDataOrFile sDataOrFile
      pure (TxOutDatumHash supp $ hashScriptDataBytes sData)
    TxOutDatumByValue sDataOrFile -> do
      sData <- firstExceptT TxCmdScriptDataError $ readScriptDataOrFile sDataOrFile
      pure (TxOutDatumInTx supp sData)
    TxOutInlineDatumByValue sDataOrFile -> do
      let cEra = alonzoEraOnwardsToCardanoEra supp
      forEraInEon cEra (txFeatureMismatch cEra TxFeatureInlineDatums) $ \babbageOnwards -> do
        sData <- firstExceptT TxCmdScriptDataError $ readScriptDataOrFile sDataOrFile
        pure $ TxOutDatumInline babbageOnwards sData

-- TODO: Currently we specify the policyId with the '--mint' option on the cli
-- and we added a separate '--policy-id' parser that parses the policy id for the
-- given reference input (since we don't have the script in this case). To avoid asking
-- for the policy id twice (in the build command) we can potentially query the UTxO and
-- access the script (and therefore the policy id).
createTxMintValue :: forall era. ShelleyBasedEra era
                  -> (Value, [ScriptWitness WitCtxMint era])
                  -> Either TxCmdError (TxMintValue BuildTx era)
createTxMintValue era (val, scriptWitnesses) =
  if List.null (valueToList val) && List.null scriptWitnesses
  then return TxMintNone
  else do
    caseShelleyToAllegraOrMaryEraOnwards
      (const (txFeatureMismatchPure (toCardanoEra era) TxFeatureMintValue))
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
    | otherwise = Left (TxCmdPolicyIdsMissing witnessesMissing (toList witnessesProvided))
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
  :: ShelleyBasedEra era
  -> (Value, [ScriptWitnessFiles WitCtxMint])
  -> ExceptT TxCmdError IO (Value, [ScriptWitness WitCtxMint era])
readValueScriptWitnesses era (v, sWitFiles) = do
  sWits <- mapM (firstExceptT TxCmdScriptWitnessError . readScriptWitness era) sWitFiles
  return (v, sWits)

-- ----------------------------------------------------------------------------
-- Transaction signing
--

runTransactionSignCmd :: ()
  => Cmd.TransactionSignCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionSignCmd
    Cmd.TransactionSignCmdArgs
      { txOrTxBodyFile = txOrTxBody
      , witnessSigningData = witnessSigningData
      , mNetworkId = mNetworkId
      , outTxFile = outTxFile
      } = do
  sks <- forM witnessSigningData $ \d ->
    lift (readWitnessSigningData d)
      & onLeft (left . TxCmdReadWitnessSigningDataError)

  let (sksByron, sksShelley) = partitionSomeWitnesses $ map categoriseSomeSigningWitness sks

  case txOrTxBody of
    InputTxFile (File inputTxFilePath) -> do
      inputTxFile <- liftIO $ fileOrPipe inputTxFilePath
      anyTx <- lift (readFileTx inputTxFile) & onLeft (left . TxCmdCddlError)

      InAnyShelleyBasedEra sbe tx <- pure anyTx

      let (txbody, existingTxKeyWits) = getTxBodyAndWitnesses tx

      byronWitnesses <-
        pure (mkShelleyBootstrapWitnesses sbe mNetworkId txbody sksByron)
          & onLeft (left . TxCmdBootstrapWitnessError)

      let newShelleyKeyWits = map (makeShelleyKeyWitness sbe txbody) sksShelley
          allKeyWits = existingTxKeyWits ++ newShelleyKeyWits ++ byronWitnesses
          signedTx = makeSignedTransaction allKeyWits txbody

      lift (writeTxFileTextEnvelopeCddl sbe outTxFile signedTx)
        & onLeft (left . TxCmdWriteFileError)

    InputTxBodyFile (File txbodyFilePath) -> do
      txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
      unwitnessed <- firstExceptT TxCmdCddlError . newExceptT
                       $ readFileTxBody txbodyFile

      case unwitnessed of
        IncompleteCddlFormattedTx anyTx -> do
         InAnyShelleyBasedEra sbe unwitTx <- pure anyTx

         let txbody = getTxBody unwitTx
         -- Byron witnesses require the network ID. This can either be provided
         -- directly or derived from a provided Byron address.
         byronWitnesses <- firstExceptT TxCmdBootstrapWitnessError
           . hoistEither
           $ mkShelleyBootstrapWitnesses sbe mNetworkId txbody sksByron

         let shelleyKeyWitnesses = map (makeShelleyKeyWitness sbe txbody) sksShelley
             tx = makeSignedTransaction (byronWitnesses ++ shelleyKeyWitnesses) txbody

         lift (writeTxFileTextEnvelopeCddl sbe outTxFile tx)
            & onLeft (left . TxCmdWriteFileError)

        UnwitnessedCliFormattedTxBody anyTxbody -> do
          InAnyShelleyBasedEra sbe txbody <- pure anyTxbody
          -- Byron witnesses require the network ID. This can either be provided
          -- directly or derived from a provided Byron address.
          byronWitnesses <- firstExceptT TxCmdBootstrapWitnessError
            . hoistEither
            $ mkShelleyBootstrapWitnesses sbe mNetworkId txbody sksByron

          let shelleyKeyWitnesses = map (makeShelleyKeyWitness sbe txbody) sksShelley
              tx = makeSignedTransaction (byronWitnesses ++ shelleyKeyWitnesses) txbody

          firstExceptT TxCmdWriteFileError . newExceptT
            $ writeLazyByteStringFile outTxFile
            $ shelleyBasedEraConstraints sbe
            $ textEnvelopeToJSON Nothing tx

-- ----------------------------------------------------------------------------
-- Transaction submission
--

runTransactionSubmitCmd :: ()
  => Cmd.TransactionSubmitCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionSubmitCmd
    Cmd.TransactionSubmitCmdArgs
      { nodeSocketPath
      , consensusModeParams
      , networkId
      , txFile
      } = do
  txFileOrPipe <- liftIO $ fileOrPipe txFile
  InAnyShelleyBasedEra era tx <- lift (readFileTx txFileOrPipe) & onLeft (left . TxCmdCddlError)
  let txInMode = TxInMode era tx
      localNodeConnInfo = LocalNodeConnectInfo
                            { localConsensusModeParams = consensusModeParams
                            , localNodeNetworkId = networkId
                            , localNodeSocketPath = nodeSocketPath
                            }

  res <- liftIO $ submitTxToNodeLocal localNodeConnInfo txInMode
  case res of
    Net.Tx.SubmitSuccess -> liftIO $ Text.putStrLn "Transaction successfully submitted."
    Net.Tx.SubmitFail reason ->
      case reason of
        TxValidationErrorInCardanoMode err -> left . TxCmdTxSubmitError . Text.pack $ show err
        TxValidationEraMismatch mismatchErr -> left $ TxCmdTxSubmitErrorEraMismatch mismatchErr

-- ----------------------------------------------------------------------------
-- Transaction fee calculation
--

runTransactionCalculateMinFeeCmd :: ()
  => Cmd.TransactionCalculateMinFeeCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionCalculateMinFeeCmd
    Cmd.TransactionCalculateMinFeeCmdArgs
      { txBodyFile = File txbodyFilePath
      , networkId = networkId
      , protocolParamsFile = protocolParamsFile
      , txInCount = TxInCount nInputs
      , txOutCount = TxOutCount nOutputs
      , txShelleyWitnessCount = TxShelleyWitnessCount nShelleyKeyWitnesses
      , txByronWitnessCount = TxByronWitnessCount nByronKeyWitnesses
      } = do

  txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
  unwitnessed <- firstExceptT TxCmdCddlError . newExceptT $ readFileTxBody txbodyFile
  pparams <- firstExceptT TxCmdProtocolParamsError $ readProtocolParameters protocolParamsFile
  case unwitnessed of
    IncompleteCddlFormattedTx anyTx -> do
      InAnyShelleyBasedEra sbe unwitTx <- pure anyTx
      let txbody =  getTxBody unwitTx
      let tx = makeSignedTransaction [] txbody
          Lovelace fee = estimateTransactionFee sbe
                            networkId
                            (protocolParamTxFeeFixed pparams)
                            (protocolParamTxFeePerByte pparams)
                            tx
                            nInputs nOutputs
                            nShelleyKeyWitnesses nByronKeyWitnesses

      liftIO $ putStrLn $ (show fee :: String) <> " Lovelace"

    UnwitnessedCliFormattedTxBody anyTxBody -> do
      InAnyShelleyBasedEra sbe txbody <- pure anyTxBody

      let tx = makeSignedTransaction [] txbody
          Lovelace fee = estimateTransactionFee sbe
                            networkId
                            (protocolParamTxFeeFixed pparams)
                            (protocolParamTxFeePerByte pparams)
                            tx
                            nInputs nOutputs
                            nByronKeyWitnesses nShelleyKeyWitnesses

      liftIO $ putStrLn $ (show fee :: String) <> " Lovelace"

-- ----------------------------------------------------------------------------
-- Transaction fee calculation
--

runTransactionCalculateMinValueCmd :: ()
  => Cmd.TransactionCalculateMinValueCmdArgs era
  -> ExceptT TxCmdError IO ()
runTransactionCalculateMinValueCmd
    Cmd.TransactionCalculateMinValueCmdArgs
      { eon
      , protocolParamsFile
      , txOut
      } = do
  pp <- firstExceptT TxCmdProtocolParamsError (readProtocolParameters protocolParamsFile)
  out <- toTxOutInShelleyBasedEra eon txOut
  -- TODO: shouldn't we just require shelley based era here instead of error-ing for byron?

  firstExceptT TxCmdPParamsErr . hoistEither
    $ checkProtocolParameters eon pp
  pp' <- hoistEither . first TxCmdProtocolParamsConverstionError $ toLedgerPParams eon pp
  let minValue = calculateMinimumUTxO eon out pp'
  liftIO . IO.print $ minValue

runTransactionPolicyIdCmd :: ()
  => Cmd.TransactionPolicyIdCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionPolicyIdCmd
    Cmd.TransactionPolicyIdCmdArgs
      { scriptFile = ScriptFile sFile
      } = do
  ScriptInAnyLang _ script <- firstExceptT TxCmdScriptFileError $
                                readFileScriptInAnyLang sFile
  liftIO . Text.putStrLn . serialiseToRawBytesHexText $ hashScript script

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
mkShelleyBootstrapWitness :: ()
  => ShelleyBasedEra era
  -> Maybe NetworkId
  -> TxBody era
  -> ShelleyBootstrapWitnessSigningKeyData
  -> Either BootstrapWitnessError (KeyWitness era)
mkShelleyBootstrapWitness _ Nothing _ (ShelleyBootstrapWitnessSigningKeyData _ Nothing) =
  Left MissingNetworkIdOrByronAddressError
mkShelleyBootstrapWitness sbe (Just nw) txBody (ShelleyBootstrapWitnessSigningKeyData skey Nothing) =
  Right $ makeShelleyBootstrapWitness sbe (WitnessNetworkId nw) txBody skey
mkShelleyBootstrapWitness sbe _ txBody (ShelleyBootstrapWitnessSigningKeyData skey (Just addr)) =
  Right $ makeShelleyBootstrapWitness sbe (WitnessByronAddress addr) txBody skey

-- | Attempt to construct Shelley bootstrap witnesses until an error is
-- encountered.
mkShelleyBootstrapWitnesses :: ()
  => ShelleyBasedEra era
  -> Maybe NetworkId
  -> TxBody era
  -> [ShelleyBootstrapWitnessSigningKeyData]
  -> Either BootstrapWitnessError [KeyWitness era]
mkShelleyBootstrapWitnesses sbe mnw txBody =
  mapM (mkShelleyBootstrapWitness sbe mnw txBody)


-- ----------------------------------------------------------------------------
-- Other misc small commands
--

runTransactionHashScriptDataCmd :: ()
  => Cmd.TransactionHashScriptDataCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionHashScriptDataCmd
    Cmd.TransactionHashScriptDataCmdArgs
      { scriptDataOrFile
      } = do
  d <- firstExceptT TxCmdScriptDataError $ readScriptDataOrFile scriptDataOrFile
  liftIO $ BS.putStrLn $ serialiseToRawBytesHex (hashScriptDataBytes d)

runTransactionTxIdCmd :: ()
  => Cmd.TransactionTxIdCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionTxIdCmd
    Cmd.TransactionTxIdCmdArgs
      { inputTxBodyOrTxFile
      } = do
  InAnyShelleyBasedEra _era txbody <-
    case inputTxBodyOrTxFile of
      InputTxBodyFile (File txbodyFilePath) -> do
        txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
        unwitnessed <- firstExceptT TxCmdCddlError . newExceptT
                          $ readFileTxBody txbodyFile
        case unwitnessed of
          UnwitnessedCliFormattedTxBody anyTxBody -> return anyTxBody
          IncompleteCddlFormattedTx (InAnyShelleyBasedEra era tx) ->
            return (InAnyShelleyBasedEra era (getTxBody tx))

      InputTxFile (File txFilePath) -> do
        txFile <- liftIO $ fileOrPipe txFilePath
        InAnyShelleyBasedEra era tx <- lift (readFileTx txFile) & onLeft (left . TxCmdCddlError)
        return . InAnyShelleyBasedEra era $ getTxBody tx

  liftIO $ BS.putStrLn $ serialiseToRawBytesHex (getTxId txbody)

runTransactionViewCmd :: ()
  => Cmd.TransactionViewCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionViewCmd
    Cmd.TransactionViewCmdArgs
      { outputFormat
      , mOutFile
      , inputTxBodyOrTxFile
      } =
  case inputTxBodyOrTxFile of
    InputTxBodyFile (File txbodyFilePath) -> do
      txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
      unwitnessed <- firstExceptT TxCmdCddlError . newExceptT
                      $ readFileTxBody txbodyFile
      InAnyShelleyBasedEra era txbody <-
        case unwitnessed of
          UnwitnessedCliFormattedTxBody anyTxBody -> pure anyTxBody
          IncompleteCddlFormattedTx (InAnyShelleyBasedEra era tx) ->
            pure $ InAnyShelleyBasedEra era (getTxBody tx)
      -- Why are we differentiating between a transaction body and a transaction?
      -- In the case of a transaction body, we /could/ simply call @makeSignedTransaction []@
      -- to get a transaction which would allow us to reuse friendlyTxBS. However,
      -- this would mean that we'd have an empty list of witnesses mentioned in the output, which
      -- is arguably not part of the transaction body.
      firstExceptT TxCmdWriteFileError . newExceptT $
        case outputFormat of
          ViewOutputFormatYaml -> friendlyTxBody FriendlyYaml mOutFile (toCardanoEra era) txbody
          ViewOutputFormatJson -> friendlyTxBody FriendlyJson mOutFile (toCardanoEra era) txbody
    InputTxFile (File txFilePath) -> do
      txFile <- liftIO $ fileOrPipe txFilePath
      InAnyShelleyBasedEra era tx <- lift (readFileTx txFile) & onLeft (left . TxCmdCddlError)
      firstExceptT TxCmdWriteFileError . newExceptT $
        case outputFormat of
          ViewOutputFormatYaml -> friendlyTx FriendlyYaml mOutFile (toCardanoEra era) tx
          ViewOutputFormatJson -> friendlyTx FriendlyJson mOutFile (toCardanoEra era) tx

-- ----------------------------------------------------------------------------
-- Witness commands
--

runTransactionWitnessCmd :: ()
  => Cmd.TransactionWitnessCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionWitnessCmd
    Cmd.TransactionWitnessCmdArgs
      { txBodyFile = File txbodyFilePath
      , witnessSigningData
      , mNetworkId
      , outFile
      } = do
  txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
  unwitnessed <- firstExceptT TxCmdCddlError . newExceptT
                   $ readFileTxBody txbodyFile
  case unwitnessed of
    IncompleteCddlFormattedTx anyTx -> do
     InAnyShelleyBasedEra sbe cddlTx <- pure anyTx

     let txbody = getTxBody cddlTx
     someWit <- firstExceptT TxCmdReadWitnessSigningDataError
                  . newExceptT $ readWitnessSigningData witnessSigningData
     witness <-
       case categoriseSomeSigningWitness someWit of
         -- Byron witnesses require the network ID. This can either be provided
         -- directly or derived from a provided Byron address.
         AByronWitness bootstrapWitData ->
           firstExceptT TxCmdBootstrapWitnessError
             . hoistEither
             $ mkShelleyBootstrapWitness sbe mNetworkId txbody bootstrapWitData
         AShelleyKeyWitness skShelley ->
           pure $ makeShelleyKeyWitness sbe txbody skShelley

     firstExceptT TxCmdWriteFileError . newExceptT
       $ writeTxWitnessFileTextEnvelopeCddl sbe outFile witness

    UnwitnessedCliFormattedTxBody anyTxbody -> do
      InAnyShelleyBasedEra sbe txbody <- pure anyTxbody

      someWit <- firstExceptT TxCmdReadWitnessSigningDataError
                   . newExceptT $ readWitnessSigningData witnessSigningData

      witness <-
        case categoriseSomeSigningWitness someWit of
          -- Byron witnesses require the network ID. This can either be provided
          -- directly or derived from a provided Byron address.
          AByronWitness bootstrapWitData ->
            firstExceptT TxCmdBootstrapWitnessError
              . hoistEither
              $ mkShelleyBootstrapWitness sbe mNetworkId txbody bootstrapWitData
          AShelleyKeyWitness skShelley ->
            pure $ makeShelleyKeyWitness sbe txbody skShelley

      firstExceptT TxCmdWriteFileError . newExceptT
        $ writeLazyByteStringFile outFile
        $ shelleyBasedEraConstraints sbe
        $ textEnvelopeToJSON Nothing witness

runTransactionSignWitnessCmd :: ()
  => Cmd.TransactionSignWitnessCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionSignWitnessCmd
    Cmd.TransactionSignWitnessCmdArgs
      { txBodyFile = File txbodyFilePath
      , witnessFiles = witnessFiles
      , outFile = outFile
      } = do
  txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
  unwitnessed <- lift (readFileTxBody txbodyFile) & onLeft (left . TxCmdCddlError)
  case unwitnessed of
    UnwitnessedCliFormattedTxBody (InAnyShelleyBasedEra era txbody) -> do
      witnesses <-
        sequence
          [ do
              InAnyShelleyBasedEra era' witness <-
                lift (readFileTxKeyWitness file) & onLeft (left . TxCmdCddlWitnessError)
              let cEra = shelleyBasedToCardanoEra era
                  cEra' = shelleyBasedToCardanoEra era'
              case testEquality era era' of
                Nothing   -> cardanoEraConstraints cEra' $ left $ TxCmdWitnessEraMismatch (AnyCardanoEra cEra) (AnyCardanoEra cEra') witnessFile
                Just Refl -> return witness
          | witnessFile@(WitnessFile file) <- witnessFiles
          ]

      let tx = makeSignedTransaction witnesses txbody

      lift (writeLazyByteStringFile outFile $ shelleyBasedEraConstraints era
             $ textEnvelopeToJSON Nothing tx) & onLeft (left . TxCmdWriteFileError)

    IncompleteCddlFormattedTx (InAnyShelleyBasedEra era anyTx) -> do
      let txbody = getTxBody anyTx
      -- TODO: Left off here. Remember we were never reading byron key witnesses anyways!
      witnesses <-
        sequence
          [ do
              InAnyShelleyBasedEra era' witness <-
                lift (readFileTxKeyWitness file) & onLeft (left . TxCmdCddlWitnessError)

              case testEquality era era' of
                Nothing   -> left $ TxCmdWitnessEraMismatch (AnyCardanoEra $ shelleyBasedToCardanoEra era) (AnyCardanoEra $ shelleyBasedToCardanoEra era') witnessFile
                Just Refl -> return witness
          | witnessFile@(WitnessFile file) <- witnessFiles ]

      let tx = makeSignedTransaction witnesses txbody

      lift (writeTxFileTextEnvelopeCddl era outFile tx) & onLeft (left . TxCmdWriteFileError)
