{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
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
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant id" #-}

{- HLINT ignore "Unused LANGUAGE pragma" -}
{- HLINT ignore "Avoid lambda using `infix`" -}

module Cardano.CLI.EraBased.Transaction.Run
  ( partitionSomeWitnesses
  , runTransactionCmds
  , runTransactionBuildCmd
  , runTransactionBuildRawCmd
  , runTransactionSignCmd
  , runTransactionSubmitCmd
  , runTransactionCalculateMinFeeCmd
  , runTransactionCalculateMinValueCmd
  , runTransactionPolicyIdCmd
  , runTransactionHashScriptDataCmd
  , runTransactionTxIdCmd
  , runTransactionWitnessCmd
  , runTransactionSignWitnessCmd
  , toTxOutByronEra
  , toTxOutInAnyEra
  )
where

import Cardano.Api
import Cardano.Api qualified as Api
import Cardano.Api.Byron qualified as Byron
import Cardano.Api.Consensus (EraMismatch (..))
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Network qualified as Consensus
import Cardano.Api.Network qualified as Net.Tx
import Cardano.Api.Shelley

import Cardano.Binary qualified as CBOR
import Cardano.CLI.EraBased.Genesis.Internal.Common (readProtocolParameters)
import Cardano.CLI.EraBased.Script.Certificate.Read
import Cardano.CLI.EraBased.Script.Certificate.Type (CertificateScriptWitness (..))
import Cardano.CLI.EraBased.Script.Mint.Read
import Cardano.CLI.EraBased.Script.Mint.Type
import Cardano.CLI.EraBased.Script.Proposal.Type (ProposalScriptWitness (..))
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Spend.Read
import Cardano.CLI.EraBased.Script.Spend.Type (SpendScriptWitness (..))
import Cardano.CLI.EraBased.Script.Vote.Type
import Cardano.CLI.EraBased.Script.Withdrawal.Read
import Cardano.CLI.EraBased.Script.Withdrawal.Type (WithdrawalScriptWitness (..))
import Cardano.CLI.EraBased.Transaction.Command
import Cardano.CLI.EraBased.Transaction.Command qualified as Cmd
import Cardano.CLI.EraBased.Transaction.Internal.HashCheck
  ( checkCertificateHashes
  , checkProposalHashes
  , checkVotingProcedureHashes
  )
import Cardano.CLI.Orphan ()
import Cardano.CLI.Read
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.NodeEraMismatchError
import Cardano.CLI.Type.Error.TxCmdError
import Cardano.CLI.Type.Error.TxValidationError
import Cardano.CLI.Type.Output (renderScriptCostsWithScriptHashesMap)
import Cardano.CLI.Type.TxFeature
import Cardano.Ledger.Api (allInputsTxBodyF, bodyTxL)
import Cardano.Prelude (putLByteString)

import Control.Monad
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString qualified as Data.Bytestring
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Containers.ListUtils (nubOrd)
import Data.Data ((:~:) (..))
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Type.Equality (TestEquality (..))
import GHC.Exts (IsList (..))
import Lens.Micro ((^.))
import System.IO qualified as IO

import Vary qualified

runTransactionCmds :: Cmd.TransactionCmds era -> ExceptT TxCmdError IO ()
runTransactionCmds = \case
  Cmd.TransactionBuildCmd args -> runTransactionBuildCmd args
  Cmd.TransactionBuildEstimateCmd args -> runTransactionBuildEstimateCmd args
  Cmd.TransactionBuildRawCmd args -> runTransactionBuildRawCmd args
  Cmd.TransactionSignCmd args -> runTransactionSignCmd args
  Cmd.TransactionSubmitCmd args -> runTransactionSubmitCmd args
  Cmd.TransactionCalculateMinFeeCmd args -> runTransactionCalculateMinFeeCmd args
  Cmd.TransactionCalculateMinValueCmd args -> runTransactionCalculateMinValueCmd args
  Cmd.TransactionCalculatePlutusScriptCostCmd args -> runTransactionCalculatePlutusScriptCostCmd args
  Cmd.TransactionHashScriptDataCmd args -> runTransactionHashScriptDataCmd args
  Cmd.TransactionTxIdCmd args -> runTransactionTxIdCmd args
  Cmd.TransactionPolicyIdCmd args -> runTransactionPolicyIdCmd args
  Cmd.TransactionWitnessCmd args -> runTransactionWitnessCmd args
  Cmd.TransactionSignWitnessCmd args -> runTransactionSignWitnessCmd args

-- ----------------------------------------------------------------------------
-- Building transactions
--

runTransactionBuildCmd
  :: Cmd.TransactionBuildCmdArgs era
  -> ExceptT TxCmdError IO ()
runTransactionBuildCmd
  Cmd.TransactionBuildCmdArgs
    { currentEra
    , nodeConnInfo =
      nodeConnInfo@LocalNodeConnectInfo
        { localNodeNetworkId = networkId
        , localNodeSocketPath = nodeSocketPath
        }
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
    , mMintedAssets
    , mValidityLowerBound
    , mValidityUpperBound
    , certificates
    , withdrawals
    , metadataSchema
    , scriptFiles
    , metadataFiles
    , mUpdateProposalFile
    , voteFiles
    , proposalFiles
    , treasuryDonation -- Maybe TxTreasuryDonation
    , isCborOutCanonical
    , buildOutputOptions
    } = do
    let eon = convert currentEra
        era' = toCardanoEra eon

    txinsAndMaybeScriptWits <-
      firstExceptT TxCmdCliSpendingScriptWitnessError $
        readSpendScriptWitnesses eon txins

    let spendingScriptWitnesses = mapMaybe (fmap sswScriptWitness . snd) txinsAndMaybeScriptWits

    certFilesAndMaybeScriptWits <-
      firstExceptT TxCmdCliScriptWitnessError $ readCertificateScriptWitnesses eon certificates

    -- TODO: Conway Era - How can we make this more composable?
    certsAndMaybeScriptWits <-
      sequence
        [ fmap
            (,cswScriptWitness <$> mSwit)
            ( firstExceptT TxCmdReadTextViewFileError . newExceptT $
                shelleyBasedEraConstraints eon $
                  readFileTextEnvelope AsCertificate (File certFile)
            )
        | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
        ]

    forM_ certsAndMaybeScriptWits (checkCertificateHashes . fst)

    withdrawalsAndMaybeScriptWits <-
      firstExceptT TxCmdCliScriptWitnessError $
        mapM (readWithdrawalScriptWitness eon) withdrawals
    txMetadata <-
      firstExceptT TxCmdMetadataError . newExceptT $
        readTxMetadata eon metadataSchema metadataFiles
    let (mintedMultiAsset, sWitFiles) = fromMaybe mempty mMintedAssets
    mintingWitnesses <-
      firstExceptT TxCmdCliScriptWitnessError (mapM (readMintScriptWitness eon) sWitFiles)
    scripts <-
      firstExceptT TxCmdScriptFileError $
        mapM (readFileScriptInAnyLang . unFile) scriptFiles
    txAuxScripts <-
      hoistEither $ first TxCmdAuxScriptsValidationError $ validateTxAuxScripts eon scripts

    mProp <- case mUpdateProposalFile of
      Just (Featured w (Just updateProposalFile)) ->
        readTxUpdateProposal w updateProposalFile & firstExceptT TxCmdReadTextViewFileError
      _ -> pure TxUpdateProposalNone

    requiredSigners <-
      mapM (firstExceptT TxCmdRequiredSignerError . newExceptT . readRequiredSigner) reqSigners
    mReturnCollateral <- forM mReturnColl $ toTxOutInShelleyBasedEra eon

    txOuts <- mapM (toTxOutInAnyEra eon) txouts

    -- Conway related
    votingProceduresAndMaybeScriptWits <-
      inEonForEra
        (pure mempty)
        (\w -> firstExceptT TxCmdVoteError $ ExceptT (readVotingProceduresFiles w voteFiles))
        era'

    forM_ votingProceduresAndMaybeScriptWits (checkVotingProcedureHashes eon . fst)

    proposals <-
      newExceptT $
        first TxCmdProposalError
          <$> readTxGovernanceActions eon proposalFiles

    forM_ proposals (checkProposalHashes eon . fst)

    -- Extract return addresses from proposals and check that the return address in each proposal is registered

    let returnAddrHashes =
          fromList
            [ stakeCred
            | (proposal, _) <- proposals
            , let (_, stakeCred, _) = fromProposalProcedure eon proposal
            ]
        treasuryWithdrawalAddresses =
          fromList
            [ stakeCred
            | (proposal, _) <- proposals
            , let (_, _, govAction) = fromProposalProcedure eon proposal
            , TreasuryWithdrawal withdrawalsList _ <- [govAction] -- Match on TreasuryWithdrawal action
            , (_, stakeCred, _) <- withdrawalsList -- Extract fund-receiving stake credentials
            ]
        allAddrHashes = Set.union returnAddrHashes treasuryWithdrawalAddresses

    (balances, _) <-
      lift
        ( executeLocalStateQueryExpr
            nodeConnInfo
            Consensus.VolatileTip
            (queryStakeAddresses eon allAddrHashes networkId)
        )
        & onLeft (left . TxCmdQueryConvenienceError . AcqFailure)
        & onLeft (left . TxCmdQueryConvenienceError . QceUnsupportedNtcVersion)
        & onLeft (left . TxCmdTxSubmitErrorEraMismatch)

    let unregisteredAddresses =
          Set.filter
            (\stakeCred -> Map.notMember (makeStakeAddress networkId stakeCred) balances)
            allAddrHashes

    unless (null unregisteredAddresses) $
      throwError $
        TxCmdUnregisteredStakeAddress unregisteredAddresses

    -- the same collateral input can be used for several plutus scripts
    let filteredTxinsc = nubOrd txinsc

    let allReferenceInputs =
          getAllReferenceInputs
            spendingScriptWitnesses
            (map mswScriptWitness mintingWitnesses)
            (mapMaybe snd certsAndMaybeScriptWits)
            (mapMaybe (\(_, _, mSwit) -> mSwit) withdrawalsAndMaybeScriptWits)
            (mapMaybe snd votingProceduresAndMaybeScriptWits)
            (mapMaybe snd proposals)
            readOnlyReferenceInputs

    let inputsThatRequireWitnessing = [input | (input, _) <- txins]
        allTxInputs = inputsThatRequireWitnessing ++ allReferenceInputs ++ filteredTxinsc

    AnyCardanoEra nodeEra <-
      lift (executeLocalStateQueryExpr nodeConnInfo Consensus.VolatileTip queryCurrentEra)
        & onLeft (left . TxCmdQueryConvenienceError . AcqFailure)
        & onLeft (left . TxCmdQueryConvenienceError . QceUnsupportedNtcVersion)

    (txEraUtxo, _, eraHistory, systemStart, _, _, _, featuredCurrentTreasuryValueM) <-
      lift
        ( executeLocalStateQueryExpr
            nodeConnInfo
            Consensus.VolatileTip
            (queryStateForBalancedTx nodeEra allTxInputs [])
        )
        & onLeft (left . TxCmdQueryConvenienceError . AcqFailure)
        & onLeft (left . TxCmdQueryConvenienceError)

    let currentTreasuryValueAndDonation =
          case (treasuryDonation, unFeatured <$> featuredCurrentTreasuryValueM) of
            (Nothing, _) -> Nothing -- We shouldn't specify the treasury value when no donation is being done
            (Just _td, Nothing) -> Nothing -- TODO: Current treasury value couldn't be obtained but is required: we should fail suggesting that the node's version is too old
            (Just td, Just ctv) -> Just (ctv, td)

    -- We need to construct the txBodycontent outside of runTxBuild
    BalancedTxBody txBodyContent balancedTxBody _ _ <-
      runTxBuild
        eon
        nodeSocketPath
        networkId
        mScriptValidity
        txinsAndMaybeScriptWits
        readOnlyReferenceInputs
        filteredTxinsc
        mReturnCollateral
        mTotalCollateral
        txOuts
        changeAddresses
        (mintedMultiAsset, mintingWitnesses)
        mValidityLowerBound
        mValidityUpperBound
        certsAndMaybeScriptWits
        withdrawalsAndMaybeScriptWits
        requiredSigners
        txAuxScripts
        txMetadata
        mProp
        mOverrideWitnesses
        votingProceduresAndMaybeScriptWits
        proposals
        currentTreasuryValueAndDonation

    -- TODO: Calculating the script cost should live as a different command.
    -- Why? Because then we can simply read a txbody and figure out
    -- the script cost vs having to build the tx body each time
    case buildOutputOptions of
      OutputScriptCostOnly fp -> do
        -- Warn that the parameter is deprecated to stderr
        liftIO $
          IO.hPutStrLn
            IO.stderr
            ( "Warning: The `--calculate-plutus-script-cost` parameter is deprecated and will be "
                <> "removed in a future version. Please use the `calculate-script-cost` command instead."
            )

        let BuildTxWith mTxProtocolParams = txProtocolParams txBodyContent

        pparams <- pure mTxProtocolParams & onNothing (left TxCmdProtocolParametersNotPresentInTxBody)
        executionUnitPrices <-
          pure (getExecutionUnitPrices era' pparams) & onNothing (left TxCmdPParamExecutionUnitsNotAvailable)

        Refl <-
          testEquality era' nodeEra
            & hoistMaybe (TxCmdTxNodeEraMismatchError $ NodeEraMismatchError era' nodeEra)

        scriptExecUnitsMap <-
          firstExceptT (TxCmdTxExecUnitsErr . AnyTxCmdTxExecUnitsErr) $
            hoistEither $
              evaluateTransactionExecutionUnits
                era'
                systemStart
                (toLedgerEpochInfo eraHistory)
                pparams
                txEraUtxo
                balancedTxBody

        scriptHashes <-
          monoidForEraInEon @AlonzoEraOnwards
            era'
            (\aeo -> pure $ collectPlutusScriptHashes aeo (makeSignedTransaction [] balancedTxBody) txEraUtxo)
            & hoistMaybe (TxCmdAlonzoEraOnwardsRequired era')

        scriptCostOutput <-
          firstExceptT TxCmdPlutusScriptCostErr $
            hoistEither $
              renderScriptCostsWithScriptHashesMap
                executionUnitPrices
                scriptHashes
                scriptExecUnitsMap
        liftIO $ LBS.writeFile (unFile fp) $ encodePretty scriptCostOutput
      OutputTxBodyOnly fpath -> cardanoEraConstraints era' . modifyError TxCmdWriteFileError $ do
        let noWitTx = makeSignedTransaction [] balancedTxBody
        hoistIOEither $
          if isCborOutCanonical == TxCborCanonical
            then writeTxFileTextEnvelopeCanonicalCddl eon fpath noWitTx
            else writeTxFileTextEnvelopeCddl eon fpath noWitTx

runTransactionBuildEstimateCmd
  :: forall era
   . Cmd.TransactionBuildEstimateCmdArgs era
  -> ExceptT TxCmdError IO ()
runTransactionBuildEstimateCmd -- TODO change type
  Cmd.TransactionBuildEstimateCmdArgs
    { currentEra
    , mScriptValidity
    , shelleyWitnesses
    , mByronWitnesses
    , protocolParamsFile
    , totalUTxOValue
    , txins
    , readOnlyReferenceInputs = readOnlyRefIns
    , requiredSigners = reqSigners
    , txinsc = txInsCollateral
    , mReturnCollateral = mReturnColl
    , txouts
    , changeAddress = TxOutChangeAddress changeAddr
    , mMintedAssets
    , mValidityLowerBound
    , mValidityUpperBound
    , certificates
    , withdrawals
    , metadataSchema
    , scriptFiles
    , metadataFiles
    , mUpdateProposalFile
    , voteFiles
    , proposalFiles
    , plutusCollateral
    , totalReferenceScriptSize
    , currentTreasuryValueAndDonation
    , isCborOutCanonical
    , txBodyOutFile
    } = do
    let sbe = convert currentEra
        meo = convert (convert currentEra :: BabbageEraOnwards era)

    ledgerPParams <-
      firstExceptT TxCmdProtocolParamsError $ readProtocolParameters sbe protocolParamsFile

    txInsAndMaybeScriptWits <-
      firstExceptT TxCmdCliSpendingScriptWitnessError $
        readSpendScriptWitnesses sbe txins

    certFilesAndMaybeScriptWits <-
      firstExceptT TxCmdCliScriptWitnessError $
        readCertificateScriptWitnesses sbe certificates

    withdrawalsAndMaybeScriptWits <-
      firstExceptT TxCmdCliScriptWitnessError $
        mapM (readWithdrawalScriptWitness sbe) withdrawals
    txMetadata <-
      firstExceptT TxCmdMetadataError
        . newExceptT
        $ readTxMetadata sbe metadataSchema metadataFiles

    let (mas, sWitFiles) = fromMaybe mempty mMintedAssets
    valuesWithScriptWits <-
      (mas,) <$> firstExceptT TxCmdCliScriptWitnessError (mapM (readMintScriptWitness sbe) sWitFiles)

    scripts <-
      firstExceptT TxCmdScriptFileError $
        mapM (readFileScriptInAnyLang . unFile) scriptFiles
    txAuxScripts <-
      hoistEither $ first TxCmdAuxScriptsValidationError $ validateTxAuxScripts sbe scripts

    txUpdateProposal <- case mUpdateProposalFile of
      Just (Featured w (Just updateProposalFile)) ->
        readTxUpdateProposal w updateProposalFile & firstExceptT TxCmdReadTextViewFileError
      _ -> pure TxUpdateProposalNone

    requiredSigners <-
      mapM (firstExceptT TxCmdRequiredSignerError . newExceptT . readRequiredSigner) reqSigners

    mReturnCollateral <- mapM (toTxOutInShelleyBasedEra sbe) mReturnColl

    txOuts <- mapM (toTxOutInAnyEra sbe) txouts

    -- the same collateral input can be used for several plutus scripts
    let filteredTxinsc = nubOrd txInsCollateral

    -- Conway related
    votingProceduresAndMaybeScriptWits <-
      inEonForShelleyBasedEra
        (pure mempty)
        ( \w ->
            firstExceptT TxCmdVoteError . ExceptT $
              conwayEraOnwardsConstraints w $
                readVotingProceduresFiles w voteFiles
        )
        sbe

    proposals <-
      lift (readTxGovernanceActions sbe proposalFiles)
        & onLeft (left . TxCmdProposalError)

    certsAndMaybeScriptWits <-
      shelleyBasedEraConstraints sbe $
        sequence
          [ fmap
              (,cswScriptWitness <$> mSwit)
              ( firstExceptT TxCmdReadTextViewFileError . newExceptT $
                  readFileTextEnvelope AsCertificate (File certFile)
              )
          | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
          ]

    txBodyContent <-
      hoistEither $
        constructTxBodyContent
          sbe
          mScriptValidity
          (Just ledgerPParams)
          txInsAndMaybeScriptWits
          readOnlyRefIns
          filteredTxinsc
          mReturnCollateral
          Nothing -- TODO: Remove total collateral parameter from estimateBalancedTxBody
          txOuts
          mValidityLowerBound
          mValidityUpperBound
          valuesWithScriptWits
          certsAndMaybeScriptWits
          withdrawalsAndMaybeScriptWits
          requiredSigners
          0
          txAuxScripts
          txMetadata
          txUpdateProposal
          votingProceduresAndMaybeScriptWits
          proposals
          currentTreasuryValueAndDonation
    let stakeCredentialsToDeregisterMap = fromList $ catMaybes [getStakeDeregistrationInfo cert | (cert, _) <- certsAndMaybeScriptWits]
        drepsToDeregisterMap = fromList $ catMaybes [getDRepDeregistrationInfo cert | (cert, _) <- certsAndMaybeScriptWits]
        poolsToDeregister = fromList $ catMaybes [getPoolDeregistrationInfo cert | (cert, _) <- certsAndMaybeScriptWits]
        totCol = fromMaybe 0 plutusCollateral
        pScriptExecUnits =
          fromList
            [ (sWitIndex, execUnits)
            | (sWitIndex, AnyScriptWitness (PlutusScriptWitness _ _ _ _ _ execUnits)) <-
                collectTxBodyScriptWitnesses sbe txBodyContent
            ]

    BalancedTxBody _ balancedTxBody _ _ <-
      hoistEither $
        first TxCmdFeeEstimationError $
          estimateBalancedTxBody
            meo
            txBodyContent
            ledgerPParams
            poolsToDeregister
            stakeCredentialsToDeregisterMap
            drepsToDeregisterMap
            pScriptExecUnits
            totCol
            shelleyWitnesses
            (fromMaybe 0 mByronWitnesses)
            (maybe 0 unReferenceScriptSize totalReferenceScriptSize)
            (anyAddressInShelleyBasedEra sbe changeAddr)
            totalUTxOValue

    let noWitTx = makeSignedTransaction [] balancedTxBody
    modifyError TxCmdWriteFileError $
      hoistIOEither $
        cardanoEraConstraints (toCardanoEra sbe) $
          if isCborOutCanonical == TxCborCanonical
            then writeTxFileTextEnvelopeCanonicalCddl sbe txBodyOutFile noWitTx
            else writeTxFileTextEnvelopeCddl sbe txBodyOutFile noWitTx

getPoolDeregistrationInfo
  :: Certificate era
  -> Maybe PoolId
getPoolDeregistrationInfo (ShelleyRelatedCertificate w cert) =
  shelleyToBabbageEraConstraints w $ getShelleyDeregistrationPoolId cert
getPoolDeregistrationInfo (ConwayCertificate w cert) =
  conwayEraOnwardsConstraints w $ getConwayDeregistrationPoolId cert

getShelleyDeregistrationPoolId
  :: L.ShelleyEraTxCert (ShelleyLedgerEra era)
  => L.TxCert (ShelleyLedgerEra era) ~ L.ShelleyTxCert (ShelleyLedgerEra era)
  => L.ShelleyTxCert (ShelleyLedgerEra era)
  -> Maybe PoolId
getShelleyDeregistrationPoolId cert = do
  case cert of
    L.RetirePoolTxCert poolId _ -> Just (StakePoolKeyHash poolId)
    _ -> Nothing

getConwayDeregistrationPoolId
  :: L.TxCert (ShelleyLedgerEra era) ~ L.ConwayTxCert (ShelleyLedgerEra era)
  => L.ConwayEraTxCert (ShelleyLedgerEra era)
  => L.ConwayTxCert (ShelleyLedgerEra era)
  -> Maybe PoolId
getConwayDeregistrationPoolId cert = do
  case cert of
    L.RetirePoolTxCert poolId _ -> Just (StakePoolKeyHash poolId)
    _ -> Nothing

getDRepDeregistrationInfo
  :: Certificate era
  -> Maybe (L.Credential L.DRepRole, Lovelace)
getDRepDeregistrationInfo ShelleyRelatedCertificate{} = Nothing
getDRepDeregistrationInfo (ConwayCertificate w cert) =
  conwayEraOnwardsConstraints w $ getConwayDRepDeregistrationInfo cert

getConwayDRepDeregistrationInfo
  :: L.TxCert (ShelleyLedgerEra era) ~ L.ConwayTxCert (ShelleyLedgerEra era)
  => L.ConwayEraTxCert (ShelleyLedgerEra era)
  => L.ConwayTxCert (ShelleyLedgerEra era)
  -> Maybe (L.Credential L.DRepRole, Lovelace)
getConwayDRepDeregistrationInfo = L.getUnRegDRepTxCert

getStakeDeregistrationInfo
  :: Certificate era
  -> Maybe (StakeCredential, Lovelace)
getStakeDeregistrationInfo (ShelleyRelatedCertificate w cert) =
  shelleyToBabbageEraConstraints w $ getShelleyDeregistrationInfo cert
getStakeDeregistrationInfo (ConwayCertificate w cert) =
  conwayEraOnwardsConstraints w $ getConwayDeregistrationInfo cert

-- There for no deposits required pre-conway for registering stake
-- credentials.
getShelleyDeregistrationInfo
  :: L.ShelleyEraTxCert (ShelleyLedgerEra era)
  => L.TxCert (ShelleyLedgerEra era) ~ L.ShelleyTxCert (ShelleyLedgerEra era)
  => L.ShelleyTxCert (ShelleyLedgerEra era)
  -> Maybe (StakeCredential, Lovelace)
getShelleyDeregistrationInfo cert = do
  case cert of
    L.UnRegTxCert stakeCred -> Just (fromShelleyStakeCredential stakeCred, 0)
    _ -> Nothing

getConwayDeregistrationInfo
  :: L.TxCert (ShelleyLedgerEra era) ~ L.ConwayTxCert (ShelleyLedgerEra era)
  => L.ConwayEraTxCert (ShelleyLedgerEra era)
  => L.ConwayTxCert (ShelleyLedgerEra era)
  -> Maybe (StakeCredential, Lovelace)
getConwayDeregistrationInfo cert = do
  case cert of
    L.UnRegDepositTxCert stakeCred depositRefund -> Just (fromShelleyStakeCredential stakeCred, depositRefund)
    _ -> Nothing

getExecutionUnitPrices :: CardanoEra era -> LedgerProtocolParameters era -> Maybe L.Prices
getExecutionUnitPrices cEra (LedgerProtocolParameters pp) =
  forEraInEonMaybe cEra $ \aeo ->
    alonzoEraOnwardsConstraints aeo $
      pp ^. L.ppPricesL

runTransactionBuildRawCmd
  :: ()
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
    , mMintedAssets
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
    , currentTreasuryValueAndDonation
    , isCborOutCanonical
    , txBodyOutFile
    } = do
    txInsAndMaybeScriptWits <-
      firstExceptT TxCmdCliSpendingScriptWitnessError $
        readSpendScriptWitnesses eon txIns

    certFilesAndMaybeScriptWits <-
      firstExceptT TxCmdCliScriptWitnessError $
        readCertificateScriptWitnesses eon certificates

    withdrawalsAndMaybeScriptWits <-
      firstExceptT TxCmdCliScriptWitnessError $
        mapM (readWithdrawalScriptWitness eon) withdrawals
    txMetadata <-
      firstExceptT TxCmdMetadataError
        . newExceptT
        $ readTxMetadata eon metadataSchema metadataFiles

    let (mas, sWitFiles) = fromMaybe mempty mMintedAssets
    valuesWithScriptWits <-
      (mas,) <$> firstExceptT TxCmdCliScriptWitnessError (mapM (readMintScriptWitness eon) sWitFiles)

    scripts <-
      firstExceptT TxCmdScriptFileError $
        mapM (readFileScriptInAnyLang . unFile) scriptFiles
    txAuxScripts <-
      hoistEither $ first TxCmdAuxScriptsValidationError $ validateTxAuxScripts eon scripts

    pparams <- forM mProtocolParamsFile $ \ppf ->
      firstExceptT TxCmdProtocolParamsError (readProtocolParameters eon ppf)

    let mLedgerPParams = LedgerProtocolParameters <$> pparams

    txUpdateProposal <- case mUpdateProprosalFile of
      Just (Featured w (Just updateProposalFile)) ->
        readTxUpdateProposal w updateProposalFile & firstExceptT TxCmdReadTextViewFileError
      _ -> pure TxUpdateProposalNone

    requiredSigners <-
      mapM (firstExceptT TxCmdRequiredSignerError . newExceptT . readRequiredSigner) reqSigners

    mReturnCollateral <- mapM (toTxOutInShelleyBasedEra eon) mReturnColl

    txOuts <- mapM (toTxOutInAnyEra eon) txouts

    -- the same collateral input can be used for several plutus scripts
    let filteredTxinsc = toList @(Set _) $ fromList txInsCollateral

    -- Conway related
    votingProceduresAndMaybeScriptWits <-
      inEonForShelleyBasedEra
        (pure mempty)
        ( \w ->
            firstExceptT TxCmdVoteError . ExceptT $
              conwayEraOnwardsConstraints w $
                readVotingProceduresFiles w voteFiles
        )
        eon

    proposals <-
      lift (readTxGovernanceActions eon proposalFiles)
        & onLeft (left . TxCmdProposalError)

    certsAndMaybeScriptWits <-
      shelleyBasedEraConstraints eon $
        sequence
          [ fmap
              (,cswScriptWitness <$> mSwit)
              ( firstExceptT TxCmdReadTextViewFileError . newExceptT $
                  readFileTextEnvelope AsCertificate (File certFile)
              )
          | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
          ]
    txBody <-
      hoistEither $
        runTxBuildRaw
          eon
          mScriptValidity
          txInsAndMaybeScriptWits
          readOnlyRefIns
          filteredTxinsc
          mReturnCollateral
          mTotalCollateral
          txOuts
          mValidityLowerBound
          mValidityUpperBound
          fee
          valuesWithScriptWits
          certsAndMaybeScriptWits
          withdrawalsAndMaybeScriptWits
          requiredSigners
          txAuxScripts
          txMetadata
          mLedgerPParams
          txUpdateProposal
          votingProceduresAndMaybeScriptWits
          proposals
          currentTreasuryValueAndDonation

    let noWitTx = makeSignedTransaction [] txBody
    modifyError TxCmdWriteFileError $
      hoistIOEither $
        if isCborOutCanonical == TxCborCanonical
          then writeTxFileTextEnvelopeCanonicalCddl eon txBodyOutFile noWitTx
          else writeTxFileTextEnvelopeCddl eon txBodyOutFile noWitTx

runTxBuildRaw
  :: ()
  => ShelleyBasedEra era
  -> Maybe ScriptValidity
  -- ^ Mark script as expected to pass or fail validation
  -> [(TxIn, Maybe (SpendScriptWitness era))]
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
  -> Lovelace
  -- ^ Tx fee
  -> (L.MultiAsset, [MintScriptWitnessWithPolicyId era])
  -- ^ Multi-Asset minted value(s)
  -> [(Certificate era, Maybe (ScriptWitness WitCtxStake era))]
  -- ^ Certificate with potential script witness
  -> [(StakeAddress, Lovelace, Maybe (WithdrawalScriptWitness era))]
  -> [Hash PaymentKey]
  -- ^ Required signers
  -> TxAuxScripts era
  -> TxMetadataInEra era
  -> Maybe (LedgerProtocolParameters era)
  -> TxUpdateProposal era
  -> [(VotingProcedures era, Maybe (VoteScriptWitness era))]
  -> [(Proposal era, Maybe (ProposalScriptWitness era))]
  -> Maybe (TxCurrentTreasuryValue, TxTreasuryDonation)
  -> Either TxCmdError (TxBody era)
runTxBuildRaw
  sbe
  mScriptValidity
  inputsAndMaybeScriptWits
  readOnlyRefIns
  txinsc
  mReturnCollateral
  mTotCollateral
  txouts
  mLowerBound
  mUpperBound
  fee
  valuesWithScriptWits
  certsAndMaybeSriptWits
  withdrawals
  reqSigners
  txAuxScripts
  txMetadata
  mpparams
  txUpdateProposal
  votingProcedures
  proposals
  mCurrentTreasuryValueAndDonation = do
    txBodyContent <-
      constructTxBodyContent
        sbe
        mScriptValidity
        (unLedgerProtocolParameters <$> mpparams)
        inputsAndMaybeScriptWits
        readOnlyRefIns
        txinsc
        mReturnCollateral
        mTotCollateral
        txouts
        mLowerBound
        mUpperBound
        valuesWithScriptWits
        certsAndMaybeSriptWits
        withdrawals
        reqSigners
        fee
        txAuxScripts
        txMetadata
        txUpdateProposal
        votingProcedures
        proposals
        mCurrentTreasuryValueAndDonation

    first TxCmdTxBodyError $ createTransactionBody sbe txBodyContent

constructTxBodyContent
  :: forall era
   . ShelleyBasedEra era
  -> Maybe ScriptValidity
  -> Maybe (L.PParams (ShelleyLedgerEra era))
  -> [(TxIn, Maybe (SpendScriptWitness era))]
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
  -- ^ Normal outputs
  -> Maybe SlotNo
  -- ^ Tx lower bound
  -> TxValidityUpperBound era
  -- ^ Tx upper bound
  -> (L.MultiAsset, [MintScriptWitnessWithPolicyId era])
  -- ^ Multi-Asset value(s)
  -> [(Certificate era, Maybe (ScriptWitness WitCtxStake era))]
  -- ^ Certificate with potential script witness
  -> [(StakeAddress, Lovelace, Maybe (WithdrawalScriptWitness era))]
  -- ^ Withdrawals
  -> [Hash PaymentKey]
  -- ^ Required signers
  -> Lovelace
  -- ^ Tx fee
  -> TxAuxScripts era
  -> TxMetadataInEra era
  -> TxUpdateProposal era
  -> [(VotingProcedures era, Maybe (VoteScriptWitness era))]
  -> [(Proposal era, Maybe (ProposalScriptWitness era))]
  -> Maybe (TxCurrentTreasuryValue, TxTreasuryDonation)
  -- ^ The current treasury value and the donation. This is a stop gap as the
  -- semantics of the donation and treasury value depend on the script languages
  -- being used.
  -> Either TxCmdError (TxBodyContent BuildTx era)
constructTxBodyContent
  sbe
  mScriptValidity
  mPparams
  inputsAndMaybeScriptWits
  readOnlyRefIns
  txinsc
  mReturnCollateral
  mTotCollateral
  txouts
  mLowerBound
  mUpperBound
  valuesWithScriptWits
  certsAndMaybeScriptWits
  withdrawals
  reqSigners
  fee
  txAuxScripts
  txMetadata
  txUpdateProposal
  votingProcedures
  proposals
  mCurrentTreasuryValueAndDonation =
    do
      let allReferenceInputs =
            getAllReferenceInputs
              (map sswScriptWitness $ mapMaybe snd inputsAndMaybeScriptWits)
              (map mswScriptWitness $ snd valuesWithScriptWits)
              (mapMaybe snd certsAndMaybeScriptWits)
              (mapMaybe (\(_, _, mSwit) -> mSwit) withdrawals)
              (mapMaybe snd votingProcedures)
              (mapMaybe snd proposals)
              readOnlyRefIns

      validatedCollateralTxIns <- validateTxInsCollateral sbe txinsc
      validatedRefInputs <- validateTxInsReference sbe allReferenceInputs
      validatedTotCollateral <-
        first TxCmdNotSupportedInEraValidationError $ validateTxTotalCollateral sbe mTotCollateral
      validatedRetCol <-
        first TxCmdNotSupportedInEraValidationError $ validateTxReturnCollateral sbe mReturnCollateral
      let txFee = TxFeeExplicit sbe fee
      validatedLowerBound <-
        first TxCmdNotSupportedInEraValidationError $ validateTxValidityLowerBound sbe mLowerBound
      validatedReqSigners <-
        first TxCmdNotSupportedInEraValidationError $ validateRequiredSigners sbe reqSigners
      validatedMintValue <- createTxMintValue sbe valuesWithScriptWits
      validatedTxScriptValidity <-
        first TxCmdNotSupportedInEraValidationError $ validateTxScriptValidity sbe mScriptValidity
      validatedVotingProcedures :: TxVotingProcedures BuildTx era <-
        first (TxCmdTxGovDuplicateVotes . TxGovDuplicateVotes) $
          mkTxVotingProcedures [(v, vswScriptWitness <$> mSwit) | (v, mSwit) <- votingProcedures]
      let txProposals = forShelleyBasedEraInEonMaybe sbe $ \w -> do
            let txp :: TxProposalProcedures BuildTx era
                txp =
                  conwayEraOnwardsConstraints w $
                    mkTxProposalProcedures $
                      [(prop, pswScriptWitness <$> mSwit) | (Proposal prop, mSwit) <- proposals]
            Featured w txp
      validatedCurrentTreasuryValue <-
        first
          TxCmdNotSupportedInEraValidationError
          (validateTxCurrentTreasuryValue sbe (fst <$> mCurrentTreasuryValueAndDonation))
      validatedTreasuryDonation <-
        first
          TxCmdNotSupportedInEraValidationError
          (validateTxTreasuryDonation sbe (snd <$> mCurrentTreasuryValueAndDonation))
      return $
        shelleyBasedEraConstraints
          sbe
          ( defaultTxBodyContent sbe
              & setTxIns (validateTxIns inputsAndMaybeScriptWits)
              & setTxInsCollateral validatedCollateralTxIns
              & setTxInsReference validatedRefInputs
              & setTxOuts txouts
              & setTxTotalCollateral validatedTotCollateral
              & setTxReturnCollateral validatedRetCol
              & setTxFee txFee
              & setTxValidityLowerBound validatedLowerBound
              & setTxValidityUpperBound mUpperBound
              & setTxMetadata txMetadata
              & setTxAuxScripts txAuxScripts
              & setTxExtraKeyWits validatedReqSigners
              & setTxProtocolParams (BuildTxWith $ LedgerProtocolParameters <$> mPparams)
              & setTxWithdrawals (TxWithdrawals sbe $ map convertWithdrawals withdrawals)
              & setTxCertificates (mkTxCertificates sbe certsAndMaybeScriptWits)
              & setTxUpdateProposal txUpdateProposal
              & setTxMintValue validatedMintValue
              & setTxScriptValidity validatedTxScriptValidity
              & setTxVotingProcedures (mkFeatured validatedVotingProcedures)
              & setTxProposalProcedures txProposals
              & setTxCurrentTreasuryValue validatedCurrentTreasuryValue
              & setTxTreasuryDonation validatedTreasuryDonation
          )
   where
    convertWithdrawals
      :: (StakeAddress, Lovelace, Maybe (WithdrawalScriptWitness era))
      -> (StakeAddress, Lovelace, BuildTxWith BuildTx (Witness WitCtxStake era))
    convertWithdrawals (sAddr, ll, mScriptWitnessFiles) =
      case mScriptWitnessFiles of
        Just sWit -> (sAddr, ll, BuildTxWith $ ScriptWitness ScriptWitnessForStakeAddr $ wswScriptWitness sWit)
        Nothing -> (sAddr, ll, BuildTxWith $ KeyWitness KeyWitnessForStakeAddr)

runTxBuild
  :: ()
  => ShelleyBasedEra era
  -> SocketPath
  -> NetworkId
  -> Maybe ScriptValidity
  -- ^ Mark script as expected to pass or fail validation
  -> [(TxIn, Maybe (SpendScriptWitness era))]
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
  -> (L.MultiAsset, [MintScriptWitnessWithPolicyId era])
  -- ^ Multi-Asset value(s)
  -> Maybe SlotNo
  -- ^ Tx lower bound
  -> TxValidityUpperBound era
  -- ^ Tx upper bound
  -> [(Certificate era, Maybe (ScriptWitness WitCtxStake era))]
  -- ^ Certificate with potential script witness
  -> [(StakeAddress, Lovelace, Maybe (WithdrawalScriptWitness era))]
  -> [Hash PaymentKey]
  -- ^ Required signers
  -> TxAuxScripts era
  -> TxMetadataInEra era
  -> TxUpdateProposal era
  -> Maybe Word
  -> [(VotingProcedures era, Maybe (VoteScriptWitness era))]
  -> [(Proposal era, Maybe (ProposalScriptWitness era))]
  -> Maybe (TxCurrentTreasuryValue, TxTreasuryDonation)
  -- ^ The current treasury value and the donation.
  -> ExceptT TxCmdError IO (BalancedTxBody era)
runTxBuild
  sbe
  socketPath
  networkId
  mScriptValidity
  inputsAndMaybeScriptWits
  readOnlyRefIns
  txinsc
  mReturnCollateral
  mTotCollateral
  txouts
  (TxOutChangeAddress changeAddr)
  mintValueWithScriptWits
  mLowerBound
  mUpperBound
  certsAndMaybeScriptWits
  withdrawals
  reqSigners
  txAuxScripts
  txMetadata
  txUpdateProposal
  mOverrideWits
  votingProcedures
  proposals
  mCurrentTreasuryValueAndDonation =
    shelleyBasedEraConstraints sbe $ do
      -- TODO: All functions should be parameterized by ShelleyBasedEra
      -- as it's not possible to call this function with ByronEra
      let era = toCardanoEra sbe
          inputsThatRequireWitnessing = [input | (input, _) <- inputsAndMaybeScriptWits]

      let allReferenceInputs =
            getAllReferenceInputs
              (map sswScriptWitness $ mapMaybe snd inputsAndMaybeScriptWits)
              (map mswScriptWitness $ snd mintValueWithScriptWits)
              (mapMaybe snd certsAndMaybeScriptWits)
              (mapMaybe (\(_, _, mSwit) -> mSwit) withdrawals)
              (mapMaybe snd votingProcedures)
              (mapMaybe snd proposals)
              readOnlyRefIns

      let allTxInputs = inputsThatRequireWitnessing ++ allReferenceInputs ++ txinsc
          localNodeConnInfo =
            LocalNodeConnectInfo
              { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
              , localNodeNetworkId = networkId
              , localNodeSocketPath = socketPath
              }

      AnyCardanoEra nodeEra <-
        lift (executeLocalStateQueryExpr localNodeConnInfo Consensus.VolatileTip queryCurrentEra)
          & onLeft (left . TxCmdQueryConvenienceError . AcqFailure)
          & onLeft (left . TxCmdQueryConvenienceError . QceUnsupportedNtcVersion)

      Refl <-
        testEquality era nodeEra
          & hoistMaybe (TxCmdTxNodeEraMismatchError $ NodeEraMismatchError era nodeEra)

      let certsToQuery = fst <$> certsAndMaybeScriptWits
      (txEraUtxo, pparams, eraHistory, systemStart, stakePools, stakeDelegDeposits, drepDelegDeposits, _) <-
        lift
          ( executeLocalStateQueryExpr localNodeConnInfo Consensus.VolatileTip $
              queryStateForBalancedTx nodeEra allTxInputs certsToQuery
          )
          & onLeft (left . TxCmdQueryConvenienceError . AcqFailure)
          & onLeft (left . TxCmdQueryConvenienceError)

      txBodyContent <-
        hoistEither $
          constructTxBodyContent
            sbe
            mScriptValidity
            (Just $ unLedgerProtocolParameters pparams)
            inputsAndMaybeScriptWits
            readOnlyRefIns
            txinsc
            mReturnCollateral
            mTotCollateral
            txouts
            mLowerBound
            mUpperBound
            mintValueWithScriptWits
            certsAndMaybeScriptWits
            withdrawals
            reqSigners
            0
            txAuxScripts
            txMetadata
            txUpdateProposal
            votingProcedures
            proposals
            mCurrentTreasuryValueAndDonation

      firstExceptT TxCmdTxInsDoNotExist
        . hoistEither
        $ txInsExistInUTxO allTxInputs txEraUtxo
      firstExceptT TxCmdQueryNotScriptLocked
        . hoistEither
        $ notScriptLockedTxIns txinsc txEraUtxo

      cAddr <-
        pure (anyAddressInEra era changeAddr)
          & onLeft (error $ "runTxBuild: Byron address used: " <> show changeAddr) -- should this throw instead?
      balancedTxBody@(BalancedTxBody _ _ _ fee) <-
        firstExceptT (TxCmdBalanceTxBody . AnyTxBodyErrorAutoBalance)
          . hoistEither
          $ makeTransactionBodyAutoBalance
            sbe
            systemStart
            (toLedgerEpochInfo eraHistory)
            pparams
            stakePools
            stakeDelegDeposits
            drepDelegDeposits
            txEraUtxo
            txBodyContent
            cAddr
            mOverrideWits

      liftIO . putStrLn . docToString $ "Estimated transaction fee:" <+> pretty fee

      return balancedTxBody

-- ----------------------------------------------------------------------------
-- Transaction body validation and conversion
--

txFeatureMismatch
  :: ()
  => Monad m
  => CardanoEra era
  -> TxFeature
  -> ExceptT TxCmdError m a
txFeatureMismatch era feature =
  hoistEither . Left $ TxCmdTxFeatureMismatch (anyCardanoEra era) feature

txFeatureMismatchPure
  :: CardanoEra era
  -> TxFeature
  -> Either TxCmdError a
txFeatureMismatchPure era feature =
  Left (TxCmdTxFeatureMismatch (anyCardanoEra era) feature)

validateTxIns
  :: [(TxIn, Maybe (SpendScriptWitness era))]
  -> [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
validateTxIns = map convertTxIn
 where
  convertTxIn
    :: (TxIn, Maybe (SpendScriptWitness era))
    -> (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))
  convertTxIn (txin, mScriptWitness) =
    case mScriptWitness of
      Just sWit ->
        (txin, BuildTxWith $ ScriptWitness ScriptWitnessForSpending $ sswScriptWitness sWit)
      Nothing ->
        (txin, BuildTxWith $ KeyWitness KeyWitnessForSpending)

validateTxInsCollateral
  :: ShelleyBasedEra era
  -> [TxIn]
  -> Either TxCmdError (TxInsCollateral era)
validateTxInsCollateral _ [] = return TxInsCollateralNone
validateTxInsCollateral era txins = do
  forShelleyBasedEraInEonMaybe era (\supported -> TxInsCollateral supported txins)
    & maybe (txFeatureMismatchPure (toCardanoEra era) TxFeatureCollateral) Right

validateTxInsReference
  :: ShelleyBasedEra era
  -> [TxIn]
  -> Either TxCmdError (TxInsReference era)
validateTxInsReference _ [] = return TxInsReferenceNone
validateTxInsReference sbe allRefIns = do
  forShelleyBasedEraInEonMaybe sbe (\supported -> TxInsReference supported allRefIns)
    & maybe (txFeatureMismatchPure (toCardanoEra sbe) TxFeatureReferenceInputs) Right

getAllReferenceInputs
  :: [ScriptWitness WitCtxTxIn era]
  -> [ScriptWitness WitCtxMint era]
  -> [ScriptWitness WitCtxStake era]
  -> [WithdrawalScriptWitness era]
  -> [VoteScriptWitness era]
  -> [ProposalScriptWitness era]
  -> [TxIn]
  -- ^ Read only reference inputs
  -> [TxIn]
getAllReferenceInputs
  spendingWitnesses
  mintWitnesses
  certScriptWitnesses
  withdrawals
  votingProceduresAndMaybeScriptWits
  propProceduresAnMaybeScriptWits
  readOnlyRefIns = do
    let txinsWitByRefInputs = map getScriptWitnessReferenceInput spendingWitnesses
        mintingRefInputs = map getScriptWitnessReferenceInput mintWitnesses
        certsWitByRefInputs = map getScriptWitnessReferenceInput certScriptWitnesses
        withdrawalsWitByRefInputs = map (getScriptWitnessReferenceInput . wswScriptWitness) withdrawals
        votesWitByRefInputs = map (getScriptWitnessReferenceInput . vswScriptWitness) votingProceduresAndMaybeScriptWits
        propsWitByRefInputs = map (getScriptWitnessReferenceInput . pswScriptWitness) propProceduresAnMaybeScriptWits

    concatMap
      catMaybes
      [ txinsWitByRefInputs
      , mintingRefInputs
      , certsWitByRefInputs
      , withdrawalsWitByRefInputs
      , votesWitByRefInputs
      , propsWitByRefInputs
      , map Just readOnlyRefIns
      ]

toAddressInAnyEra
  :: CardanoEra era
  -> AddressAny
  -> Either TxCmdError (AddressInEra era)
toAddressInAnyEra era addrAny = runExcept $ do
  case addrAny of
    AddressByron bAddr -> pure (AddressInEra ByronAddressInAnyEra bAddr)
    AddressShelley sAddr -> do
      sbe <-
        requireShelleyBasedEra era
          & onNothing (txFeatureMismatch era TxFeatureShelleyAddresses)

      pure (AddressInEra (ShelleyAddressInEra sbe) sAddr)

toAddressInShelleyBasedEra
  :: ShelleyBasedEra era
  -> Address ShelleyAddr
  -> Either TxCmdError (AddressInEra era)
toAddressInShelleyBasedEra sbe sAddr =
  runExcept $
    pure (AddressInEra (ShelleyAddressInEra sbe) sAddr)

toTxOutValueInAnyEra
  :: ShelleyBasedEra era
  -> Value
  -> Either TxCmdError (TxOutValue era)
toTxOutValueInAnyEra era val =
  caseShelleyToAllegraOrMaryEraOnwards
    ( \_ -> case valueToLovelace val of
        Just l -> return (TxOutValueShelleyBased era l)
        Nothing -> txFeatureMismatchPure (toCardanoEra era) TxFeatureMultiAssetOutputs
    )
    (\w -> return (TxOutValueShelleyBased era (toLedgerValue w val)))
    era

toTxOutValueInShelleyBasedEra
  :: ShelleyBasedEra era
  -> Value
  -> Either TxCmdError (TxOutValue era)
toTxOutValueInShelleyBasedEra sbe val =
  caseShelleyToAllegraOrMaryEraOnwards
    ( \_ -> case valueToLovelace val of
        Just l -> return (TxOutValueShelleyBased sbe l)
        Nothing -> txFeatureMismatchPure (toCardanoEra sbe) TxFeatureMultiAssetOutputs
    )
    (\w -> return (TxOutValueShelleyBased sbe (toLedgerValue w val)))
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

  refScript <-
    inEonForEra
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
toTxOutInAnyEra
  :: ShelleyBasedEra era
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

  refScript <-
    caseShelleyToAlonzoOrBabbageEraOnwards
      (const (pure ReferenceScriptNone))
      (\wb -> getReferenceScript wb refScriptFp)
      era
  pure $ TxOut addr val datum refScript

getReferenceScript
  :: ()
  => BabbageEraOnwards era
  -> ReferenceScriptAnyEra
  -> ExceptT TxCmdError IO (ReferenceScript era)
getReferenceScript w = \case
  ReferenceScriptAnyEraNone -> return ReferenceScriptNone
  ReferenceScriptAnyEra fp -> ReferenceScript w <$> firstExceptT TxCmdScriptFileError (readFileScriptInAnyLang fp)

toTxAlonzoDatum
  :: ()
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
      pure (TxOutSupplementalDatum supp sData)
    TxOutInlineDatumByValue sDataOrFile -> do
      let cEra = toCardanoEra supp
      forEraInEon cEra (txFeatureMismatch cEra TxFeatureInlineDatums) $ \babbageOnwards -> do
        sData <- firstExceptT TxCmdScriptDataError $ readScriptDataOrFile sDataOrFile
        pure $ TxOutDatumInline babbageOnwards sData

-- TODO: Currently we specify the policyId with the '--mint' option on the cli
-- and we added a separate '--policy-id' parser that parses the policy id for the
-- given reference input (since we don't have the script in this case). To avoid asking
-- for the policy id twice (in the build command) we can potentially query the UTxO and
-- access the script (and therefore the policy id).
createTxMintValue
  :: forall era
   . ShelleyBasedEra era
  -> (L.MultiAsset, [MintScriptWitnessWithPolicyId era])
  -> Either TxCmdError (TxMintValue BuildTx era)
createTxMintValue era (val, scriptWitnesses) =
  if mempty == val && List.null scriptWitnesses
    then return TxMintNone
    else do
      caseShelleyToAllegraOrMaryEraOnwards
        (const (txFeatureMismatchPure (toCardanoEra era) TxFeatureMintValue))
        ( \w -> do
            let policiesWithAssets :: Map PolicyId PolicyAssets
                policiesWithAssets = multiAssetToPolicyAssets val
                -- The set of policy ids for which we need witnesses:
                witnessesNeededSet :: Set PolicyId
                witnessesNeededSet = Map.keysSet policiesWithAssets

                witnessesProvidedMap :: Map PolicyId (ScriptWitness WitCtxMint era)
                witnessesProvidedMap = fromList $ [(polid, sWit) | MintScriptWitnessWithPolicyId polid sWit <- scriptWitnesses]

                witnessesProvidedSet :: Set PolicyId
                witnessesProvidedSet = Map.keysSet witnessesProvidedMap
            -- Check not too many, nor too few:
            validateAllWitnessesProvided witnessesNeededSet witnessesProvidedSet
            validateNoUnnecessaryWitnesses witnessesNeededSet witnessesProvidedSet
            pure $
              TxMintValue w $
                Map.intersectionWith
                  (\assets wit -> (assets, BuildTxWith wit))
                  policiesWithAssets
                  witnessesProvidedMap
        )
        era
 where
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

-- ----------------------------------------------------------------------------
-- Transaction signing
--

runTransactionSignCmd
  :: ()
  => Cmd.TransactionSignCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionSignCmd
  Cmd.TransactionSignCmdArgs
    { txOrTxBodyFile = txOrTxBody
    , witnessSigningData
    , mNetworkId
    , isCborOutCanonical
    , outTxFile
    } = do
    sks <- forM witnessSigningData $ \d ->
      lift (readWitnessSigningData d)
        & onLeft (left . TxCmdReadWitnessSigningDataError)

    let (sksByron, sksShelley) = partitionSomeWitnesses $ map categoriseSomeSigningWitness sks

    case txOrTxBody of
      InputTxFile (File inputTxFilePath) -> do
        inputTxFile <- liftIO $ fileOrPipe inputTxFilePath
        anyTx <- lift (readFileTx inputTxFile) & onLeft (left . TxCmdTextEnvCddlError)

        InAnyShelleyBasedEra sbe tx@(ShelleyTx _ ledgerTx) <- pure anyTx

        let (apiTxBody, existingTxKeyWits) = getTxBodyAndWitnesses tx

        byronWitnesses <-
          firstExceptT TxCmdBootstrapWitnessError . liftEither $
            forM sksByron $
              shelleyBasedEraConstraints sbe $
                mkShelleyBootstrapWitness sbe mNetworkId (ledgerTx ^. L.bodyTxL)

        let newShelleyKeyWits = map (makeShelleyKeyWitness sbe apiTxBody) sksShelley
            allKeyWits = existingTxKeyWits ++ newShelleyKeyWits ++ byronWitnesses
            signedTx = makeSignedTransaction allKeyWits apiTxBody

        modifyError TxCmdWriteFileError $
          hoistIOEither $
            if isCborOutCanonical == TxCborCanonical
              then writeTxFileTextEnvelopeCanonicalCddl sbe outTxFile signedTx
              else writeTxFileTextEnvelopeCddl sbe outTxFile signedTx
      InputTxBodyFile (File txbodyFilePath) -> do
        txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
        unwitnessed <-
          firstExceptT TxCmdTextEnvCddlError . newExceptT $
            readFileTxBody txbodyFile

        case unwitnessed of
          IncompleteCddlTxBody anyTxBody -> do
            InAnyShelleyBasedEra sbe txbody@(ShelleyTxBody _ ledgerTxBody _ _ _ _) <- pure anyTxBody

            -- Byron witnesses require the network ID. This can either be provided
            -- directly or derived from a provided Byron address.
            byronWitnesses <-
              firstExceptT TxCmdBootstrapWitnessError . liftEither $
                forM sksByron $
                  mkShelleyBootstrapWitness sbe mNetworkId ledgerTxBody

            let shelleyKeyWitnesses = map (makeShelleyKeyWitness sbe txbody) sksShelley
                tx = makeSignedTransaction (byronWitnesses ++ shelleyKeyWitnesses) txbody

            modifyError TxCmdWriteFileError $
              hoistIOEither $
                if isCborOutCanonical == TxCborCanonical
                  then writeTxFileTextEnvelopeCanonicalCddl sbe outTxFile tx
                  else writeTxFileTextEnvelopeCddl sbe outTxFile tx

-- ----------------------------------------------------------------------------
-- Transaction submission
--

runTransactionSubmitCmd
  :: ()
  => Cmd.TransactionSubmitCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionSubmitCmd
  Cmd.TransactionSubmitCmdArgs
    { nodeConnInfo
    , txFile
    } = do
    txFileOrPipe <- liftIO $ fileOrPipe txFile
    InAnyShelleyBasedEra era tx <-
      lift (readFileTx txFileOrPipe) & onLeft (left . TxCmdTextEnvCddlError)
    let txInMode = TxInMode era tx
    res <- liftIO $ submitTxToNodeLocal nodeConnInfo txInMode
    case res of
      Net.Tx.SubmitSuccess -> do
        liftIO $ Text.hPutStrLn IO.stderr "Transaction successfully submitted. Transaction hash is:"
        liftIO $ LBS.putStrLn $ Aeson.encode $ TxSubmissionResult $ getTxId $ getTxBody tx
      Net.Tx.SubmitFail reason ->
        case reason of
          TxValidationErrorInCardanoMode err -> left . TxCmdTxSubmitError . Text.pack $ show err
          TxValidationEraMismatch mismatchErr -> left $ TxCmdTxSubmitErrorEraMismatch mismatchErr

-- ----------------------------------------------------------------------------
-- Transaction fee calculation
--

runTransactionCalculateMinFeeCmd
  :: ()
  => Cmd.TransactionCalculateMinFeeCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionCalculateMinFeeCmd
  Cmd.TransactionCalculateMinFeeCmdArgs
    { txBodyFile = File txbodyFilePath
    , protocolParamsFile = protocolParamsFile
    , txShelleyWitnessCount = TxShelleyWitnessCount nShelleyKeyWitnesses
    , txByronWitnessCount = TxByronWitnessCount nByronKeyWitnesses
    , referenceScriptSize = ReferenceScriptSize sReferenceScript
    , outputFormat
    , outFile
    } = do
    txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
    unwitnessed <-
      firstExceptT TxCmdTextEnvCddlError . newExceptT $
        readFileTxBody txbodyFile

    let nShelleyKeyWitW32 = fromIntegral nShelleyKeyWitnesses

    InAnyShelleyBasedEra sbe txbody <- pure $ unIncompleteCddlTxBody unwitnessed

    lpparams <-
      firstExceptT TxCmdProtocolParamsError $
        readProtocolParameters sbe protocolParamsFile

    let shelleyfee = evaluateTransactionFee sbe lpparams txbody nShelleyKeyWitW32 0 sReferenceScript

    let byronfee =
          shelleyBasedEraConstraints sbe $
            calculateByronWitnessFees (lpparams ^. L.ppMinFeeAL) nByronKeyWitnesses

    let fee = shelleyfee + byronfee
        textToWrite = docToText $ pretty fee
        jsonToWrite = encodePretty $ Aeson.object ["fee" .= fee]

    outputFormat
      & ( id
            . Vary.on
              ( \FormatJson -> case outFile of
                  Nothing ->
                    liftIO $ LBS.putStrLn jsonToWrite
                  Just file ->
                    firstExceptT TxCmdWriteFileError . newExceptT $ writeLazyByteStringFile file jsonToWrite
              )
            . Vary.on
              ( \FormatText -> case outFile of
                  Nothing ->
                    liftIO $ Text.putStrLn textToWrite
                  Just file ->
                    firstExceptT TxCmdWriteFileError . newExceptT $ writeTextFile file textToWrite
              )
            $ Vary.exhaustiveCase
        )

-- Extra logic to handle byron witnesses.
-- TODO: move this to Cardano.API.Fee.evaluateTransactionFee.
calculateByronWitnessFees
  :: ()
  => Lovelace
  -- ^ The tx fee per byte (from protocol parameters)
  -> Int
  -- ^ The number of Byron key witnesses
  -> Lovelace
calculateByronWitnessFees txFeePerByte byronwitcount =
  L.Coin $
    toInteger txFeePerByte
      * toInteger byronwitcount
      * toInteger sizeByronKeyWitnesses
 where
  sizeByronKeyWitnesses = smallArray + keyObj + sigObj + ccodeObj + attrsObj

  smallArray = 1

  keyObj = 2 + keyLen
  keyLen = 32

  sigObj = 2 + sigLen
  sigLen = 64

  ccodeObj = 2 + ccodeLen
  ccodeLen = 32

  attrsObj = 2 + Data.Bytestring.length attributes

  -- We assume testnet network magic here to avoid having
  -- to thread the actual network ID into this function
  -- merely to calculate the fees of byron witnesses more accurately.
  -- This may slightly over-estimate min fees for byron witnesses
  -- in mainnet transaction by one Word32 per witness.
  attributes =
    CBOR.serialize' $
      Byron.mkAttributes
        Byron.AddrAttributes
          { Byron.aaVKDerivationPath = Nothing
          , Byron.aaNetworkMagic = Byron.NetworkTestnet maxBound
          }

-- ----------------------------------------------------------------------------
-- Transaction fee calculation
--

runTransactionCalculateMinValueCmd
  :: ()
  => Cmd.TransactionCalculateMinValueCmdArgs era
  -> ExceptT TxCmdError IO ()
runTransactionCalculateMinValueCmd
  Cmd.TransactionCalculateMinValueCmdArgs
    { eon
    , protocolParamsFile
    , txOut
    } = do
    pp <- firstExceptT TxCmdProtocolParamsError (readProtocolParameters eon protocolParamsFile)
    out <- toTxOutInShelleyBasedEra eon txOut

    let minValue = calculateMinimumUTxO eon out pp
    liftIO . IO.print $ minValue

runTransactionCalculatePlutusScriptCostCmd
  :: Cmd.TransactionCalculatePlutusScriptCostCmdArgs -> ExceptT TxCmdError IO ()
runTransactionCalculatePlutusScriptCostCmd
  Cmd.TransactionCalculatePlutusScriptCostCmdArgs
    { nodeConnInfo
    , txFileIn
    , outputFile
    } = do
    txFileOrPipeIn <- liftIO $ fileOrPipe txFileIn
    InAnyShelleyBasedEra txEra tx@(ShelleyTx sbe ledgerTx) <-
      liftIO (readFileTx txFileOrPipeIn) & onLeft (left . TxCmdTextEnvCddlError)

    let relevantTxIns :: Set TxIn
        relevantTxIns = Set.map fromShelleyTxIn $ shelleyBasedEraConstraints sbe (ledgerTx ^. bodyTxL . allInputsTxBodyF)

    (AnyCardanoEra nodeEra, systemStart, eraHistory, txEraUtxo, pparams) <-
      lift
        ( executeLocalStateQueryExpr nodeConnInfo Consensus.VolatileTip $ do
            eCurrentEra <- queryCurrentEra
            eSystemStart <- querySystemStart
            eEraHistory <- queryEraHistory
            eeUtxo <- queryUtxo txEra (QueryUTxOByTxIn relevantTxIns)
            ePp <- queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe Api.QueryProtocolParameters
            return $ do
              currentEra <- first QceUnsupportedNtcVersion eCurrentEra
              systemStart <- first QceUnsupportedNtcVersion eSystemStart
              eraHistory <- first QceUnsupportedNtcVersion eEraHistory
              utxo <- first QueryEraMismatch =<< first QceUnsupportedNtcVersion eeUtxo
              pp <- first QueryEraMismatch =<< first QceUnsupportedNtcVersion ePp
              return (currentEra, systemStart, eraHistory, utxo, LedgerProtocolParameters pp)
        )
        & onLeft (left . TxCmdQueryConvenienceError . AcqFailure)
        & onLeft (left . TxCmdQueryConvenienceError)

    Refl <-
      testEquality nodeEra (convert txEra)
        & hoistMaybe
          ( TxCmdTxSubmitErrorEraMismatch $
              EraMismatch{ledgerEraName = docToText $ pretty nodeEra, otherEraName = docToText $ pretty txEra}
          )

    calculatePlutusScriptsCosts
      (convert txEra)
      systemStart
      eraHistory
      pparams
      txEraUtxo
      tx
   where
    calculatePlutusScriptsCosts
      :: CardanoEra era
      -> SystemStart
      -> EraHistory
      -> LedgerProtocolParameters era
      -> UTxO era
      -> Tx era
      -> ExceptT TxCmdError IO ()
    calculatePlutusScriptsCosts era' systemStart eraHistory pparams txEraUtxo tx = do
      scriptHashes <-
        monoidForEraInEon @AlonzoEraOnwards
          era'
          (\aeo -> pure $ collectPlutusScriptHashes aeo tx txEraUtxo)
          & hoistMaybe (TxCmdAlonzoEraOnwardsRequired era')

      executionUnitPrices <-
        pure (getExecutionUnitPrices era' pparams) & onNothing (left TxCmdPParamExecutionUnitsNotAvailable)

      scriptExecUnitsMap <-
        firstExceptT (TxCmdTxExecUnitsErr . AnyTxCmdTxExecUnitsErr) $
          hoistEither $
            evaluateTransactionExecutionUnits
              era'
              systemStart
              (toLedgerEpochInfo eraHistory)
              pparams
              txEraUtxo
              (getTxBody tx)

      scriptCostOutput <-
        firstExceptT TxCmdPlutusScriptCostErr $
          hoistEither $
            renderScriptCostsWithScriptHashesMap
              executionUnitPrices
              scriptHashes
              scriptExecUnitsMap
      liftIO
        $ ( case outputFile of
              Just file -> LBS.writeFile (unFile file)
              Nothing -> putLByteString
          )
        $ encodePretty scriptCostOutput

runTransactionPolicyIdCmd
  :: ()
  => Cmd.TransactionPolicyIdCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionPolicyIdCmd
  Cmd.TransactionPolicyIdCmdArgs
    { scriptFile = File sFile
    } = do
    ScriptInAnyLang _ script <-
      firstExceptT TxCmdScriptFileError $
        readFileScriptInAnyLang sFile
    liftIO . Text.putStrLn . serialiseToRawBytesHexText $ hashScript script

partitionSomeWitnesses
  :: [ByronOrShelleyWitness]
  -> ( [ShelleyBootstrapWitnessSigningKeyData]
     , [ShelleyWitnessSigningKey]
     )
partitionSomeWitnesses = reversePartitionedWits . Foldable.foldl' go mempty
 where
  reversePartitionedWits (bw, skw) =
    (reverse bw, reverse skw)

  go (byronAcc, shelleyKeyAcc) byronOrShelleyWit =
    case byronOrShelleyWit of
      AByronWitness byronWit ->
        (byronWit : byronAcc, shelleyKeyAcc)
      AShelleyKeyWitness shelleyKeyWit ->
        (byronAcc, shelleyKeyWit : shelleyKeyAcc)

-- ----------------------------------------------------------------------------
-- Other misc small commands
--

runTransactionHashScriptDataCmd
  :: ()
  => Cmd.TransactionHashScriptDataCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionHashScriptDataCmd
  Cmd.TransactionHashScriptDataCmdArgs
    { scriptDataOrFile
    } = do
    d <- firstExceptT TxCmdScriptDataError $ readScriptDataOrFile scriptDataOrFile
    liftIO $ BS.putStrLn $ serialiseToRawBytesHex (hashScriptDataBytes d)

runTransactionTxIdCmd
  :: ()
  => Cmd.TransactionTxIdCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionTxIdCmd
  Cmd.TransactionTxIdCmdArgs
    { inputTxBodyOrTxFile
    , outputFormat
    } = do
    InAnyShelleyBasedEra _era txbody <-
      case inputTxBodyOrTxFile of
        InputTxBodyFile (File txbodyFilePath) -> do
          txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
          unwitnessed <-
            firstExceptT TxCmdTextEnvCddlError . newExceptT $
              readFileTxBody txbodyFile
          return $ unIncompleteCddlTxBody unwitnessed
        InputTxFile (File txFilePath) -> do
          txFile <- liftIO $ fileOrPipe txFilePath
          InAnyShelleyBasedEra era tx <- lift (readFileTx txFile) & onLeft (left . TxCmdTextEnvCddlError)
          return . InAnyShelleyBasedEra era $ getTxBody tx

    let txId = getTxId txbody

    liftIO $
      outputFormat
        & ( id
              . Vary.on (\FormatJson -> LBS.putStrLn $ Aeson.encode $ TxSubmissionResult txId)
              . Vary.on (\FormatText -> BS.putStrLn $ serialiseToRawBytesHex txId)
              $ Vary.exhaustiveCase
          )

-- ----------------------------------------------------------------------------
-- Witness commands
--

runTransactionWitnessCmd
  :: ()
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
    unwitnessed <-
      firstExceptT TxCmdTextEnvCddlError . newExceptT $
        readFileTxBody txbodyFile
    case unwitnessed of
      IncompleteCddlTxBody anyTxBody -> do
        InAnyShelleyBasedEra sbe txbody@(ShelleyTxBody _ ledgerTxBody _ _ _ _) <- pure anyTxBody
        someWit <-
          firstExceptT TxCmdReadWitnessSigningDataError
            . newExceptT
            $ readWitnessSigningData witnessSigningData
        witness <-
          case categoriseSomeSigningWitness someWit of
            -- Byron witnesses require the network ID. This can either be provided
            -- directly or derived from a provided Byron address.
            AByronWitness bootstrapWitData ->
              firstExceptT TxCmdBootstrapWitnessError . liftEither $
                mkShelleyBootstrapWitness sbe mNetworkId ledgerTxBody bootstrapWitData
            AShelleyKeyWitness skShelley ->
              pure $ makeShelleyKeyWitness sbe txbody skShelley

        firstExceptT TxCmdWriteFileError . newExceptT $
          writeTxWitnessFileTextEnvelopeCddl sbe outFile witness

runTransactionSignWitnessCmd
  :: ()
  => Cmd.TransactionSignWitnessCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionSignWitnessCmd
  Cmd.TransactionSignWitnessCmdArgs
    { txBodyFile = File txbodyFilePath
    , witnessFiles
    , outFile
    , isCborOutCanonical
    } = do
    txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
    -- unwitnessed body
    IncompleteCddlTxBody (InAnyShelleyBasedEra era txbody) <-
      lift (readFileTxBody txbodyFile) & onLeft (left . TxCmdTextEnvCddlError)
    witnesses <-
      sequence
        [ do
            InAnyShelleyBasedEra era' witness <-
              lift (readFileTxKeyWitness file) & onLeft (left . TxCmdCddlWitnessError)

            case testEquality era era' of
              Nothing ->
                left $
                  TxCmdWitnessEraMismatch
                    (AnyCardanoEra $ toCardanoEra era)
                    (AnyCardanoEra $ toCardanoEra era')
                    witnessFile
              Just Refl -> return witness
        | witnessFile@(WitnessFile file) <- witnessFiles
        ]

    let tx = makeSignedTransaction witnesses txbody
    modifyError TxCmdWriteFileError $
      hoistIOEither $
        if isCborOutCanonical == TxCborCanonical
          then writeTxFileTextEnvelopeCanonicalCddl era outFile tx
          else writeTxFileTextEnvelopeCddl era outFile tx
