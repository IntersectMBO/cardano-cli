{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

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
  )
where

import Cardano.Api hiding
  ( Certificate
  , mkTxCertificates
  , txId
  , validateTxIns
  , validateTxInsCollateral
  )
import Cardano.Api qualified as Api
import Cardano.Api.Byron qualified as Byron
import Cardano.Api.Experimental (obtainCommonConstraints)
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.AnyScript qualified as Exp
import Cardano.Api.Experimental.AnyScriptWitness qualified as Exp
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Network qualified as Consensus
import Cardano.Api.Network qualified as Net.Tx

import Cardano.Binary qualified as CBOR
import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Compatible.Transaction.TxOut
import Cardano.CLI.EraBased.Genesis.Internal.Common (readProtocolParameters)
import Cardano.CLI.EraBased.Script.Certificate.Read
import Cardano.CLI.EraBased.Script.Mint.Read
import Cardano.CLI.EraBased.Script.Proposal.Read
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Spend.Read
import Cardano.CLI.EraBased.Script.Vote.Read
import Cardano.CLI.EraBased.Script.Withdrawal.Read
import Cardano.CLI.EraBased.Transaction.Command
import Cardano.CLI.EraBased.Transaction.Command qualified as Cmd
import Cardano.CLI.EraBased.Transaction.Internal.HashCheck
  ( checkCertificateHashes
  , checkProposalHashes
  , checkVotingProcedureHashes
  )
import Cardano.CLI.Json.Encode qualified as Json
import Cardano.CLI.Orphan ()
import Cardano.CLI.Read
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.NodeEraMismatchError
import Cardano.CLI.Type.Error.ProtocolParamsError
import Cardano.CLI.Type.Error.TxCmdError
import Cardano.CLI.Type.Error.TxValidationError
import Cardano.CLI.Type.Output (renderScriptCostsWithScriptHashesMap)
import Cardano.Ledger.Api (allInputsTxBodyF, bodyTxL)
import Cardano.Prelude (putLByteString)

import RIO hiding (toList)

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString qualified as Data.Bytestring
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Data ((:~:) (..))
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Type.Equality (TestEquality (..))
import Data.Universe (Some)
import GHC.Exts (IsList (..))
import System.IO qualified as IO
import Vary qualified

runTransactionCmds :: Exp.IsEra era => Cmd.TransactionCmds era -> CIO e ()
runTransactionCmds = \case
  Cmd.TransactionBuildCmd args ->
    runTransactionBuildCmd args
  Cmd.TransactionBuildEstimateCmd args -> runTransactionBuildEstimateCmd args
  Cmd.TransactionBuildRawCmd args ->
    runTransactionBuildRawCmd args
  Cmd.TransactionSignCmd args -> fromExceptTCli $ runTransactionSignCmd args
  Cmd.TransactionSubmitCmd args -> fromExceptTCli $ runTransactionSubmitCmd args
  Cmd.TransactionCalculateMinFeeCmd args ->
    runTransactionCalculateMinFeeCmd args
  Cmd.TransactionCalculateMinValueCmd args ->
    runTransactionCalculateMinValueCmd args
  Cmd.TransactionCalculatePlutusScriptCostCmd args -> fromExceptTCli $ runTransactionCalculatePlutusScriptCostCmd args
  Cmd.TransactionHashScriptDataCmd args -> fromExceptTCli $ runTransactionHashScriptDataCmd args
  Cmd.TransactionTxIdCmd args -> fromExceptTCli $ runTransactionTxIdCmd args
  Cmd.TransactionPolicyIdCmd args ->
    runTransactionPolicyIdCmd args
  Cmd.TransactionWitnessCmd args -> fromExceptTCli $ runTransactionWitnessCmd args
  Cmd.TransactionSignWitnessCmd args -> fromExceptTCli $ runTransactionSignWitnessCmd args

-- ----------------------------------------------------------------------------
-- Building transactions
--

runTransactionBuildCmd
  :: Exp.IsEra era
  => Cmd.TransactionBuildCmdArgs era
  -> CIO e ()
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
      readSpendScriptWitnesses txins

    let spendingScriptWitnesses = map snd txinsAndMaybeScriptWits

    certFilesAndMaybeScriptWits <-
      readCertificateScriptWitnesses certificates

    -- TODO: Conway Era - How can we make this more composable?
    certsAndMaybeScriptWits
      :: [(Exp.Certificate (Exp.LedgerEra era), Exp.AnyWitness (Exp.LedgerEra era))] <-
      sequence
        [ (,mSwit)
            <$> ( fromEitherIOCli @(FileError TextEnvelopeError) $
                    obtainCommonConstraints currentEra $
                      readFileTextEnvelope (File certFile)
                )
        | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
        ]

    forM_ certsAndMaybeScriptWits (fromExceptTCli . checkCertificateHashes . fst)

    withdrawalsAndMaybeScriptWits <-
      mapM readWithdrawalScriptWitness withdrawals
    txMetadata <-
      readTxMetadata currentEra metadataSchema metadataFiles
    let (mintedMultiAsset, sWitFiles) = fromMaybe mempty mMintedAssets
    mintingWitnesses <-
      mapM readMintScriptWitness sWitFiles
    scripts <-
      mapM (readFileScriptInAnyLang . unFile) scriptFiles
    txAuxScripts <-
      fromEitherCli $ validateTxAuxScripts scripts

    mProp <- case mUpdateProposalFile of
      Just (Featured w (Just updateProposalFile)) ->
        readTxUpdateProposal w updateProposalFile & fromExceptTCli
      _ -> pure TxUpdateProposalNone

    requiredSigners <-
      mapM (fromEitherIOCli . readRequiredSigner) reqSigners
    mReturnCollateral <- forM mReturnColl toTxOutInShelleyBasedEra

    txOuts <- mapM (toTxOutInAnyEra eon) txouts

    -- Conway related
    votingProceduresAndMaybeScriptWits :: [(VotingProcedures era, Exp.AnyWitness (Exp.LedgerEra era))] <-
      readVotingProceduresFiles voteFiles

    forM_ votingProceduresAndMaybeScriptWits (fromExceptTCli . checkVotingProcedureHashes . fst)

    proposals <-
      readTxGovernanceActions proposalFiles

    forM_ proposals (fromExceptTCli . checkProposalHashes . fst)

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
      fromEitherIOCli
        ( executeLocalStateQueryExpr
            nodeConnInfo
            Consensus.VolatileTip
            (queryStakeAddresses eon allAddrHashes networkId)
        )
        & fromEitherCIOCli
        & fromEitherCIOCli

    let unregisteredAddresses =
          Set.filter
            (\stakeCred -> Map.notMember (makeStakeAddress networkId stakeCred) balances)
            allAddrHashes

    unless (null unregisteredAddresses) $
      throwCliError $
        TxCmdUnregisteredStakeAddress unregisteredAddresses

    -- the same collateral input can be used for several plutus scripts
    let filteredTxinsc = nubOrd txinsc

    let allReferenceInputs =
          getAllReferenceInputs
            spendingScriptWitnesses
            (map snd mintingWitnesses)
            (map snd certsAndMaybeScriptWits)
            (map (\(_, _, wit) -> wit) withdrawalsAndMaybeScriptWits)
            (map snd votingProceduresAndMaybeScriptWits)
            (map snd proposals)
            readOnlyReferenceInputs

    let inputsThatRequireWitnessing = [input | (input, _) <- txins]
        allTxInputs = inputsThatRequireWitnessing ++ allReferenceInputs ++ filteredTxinsc

    AnyCardanoEra nodeEra <-
      fromEitherIOCli (executeLocalStateQueryExpr nodeConnInfo Consensus.VolatileTip queryCurrentEra)
        & fromEitherCIOCli

    (txEraUtxo, _, eraHistory, systemStart, _, _, _, featuredCurrentTreasuryValueM) <-
      fromEitherIOCli
        ( executeLocalStateQueryExpr
            nodeConnInfo
            Consensus.VolatileTip
            (queryStateForBalancedTx nodeEra allTxInputs [])
        )
        & fromEitherCIOCli

    let currentTreasuryValueAndDonation =
          case (treasuryDonation, unFeatured <$> featuredCurrentTreasuryValueM) of
            (Nothing, _) -> Nothing -- We shouldn't specify the treasury value when no donation is being done
            (Just _td, Nothing) -> Nothing -- TODO: Current treasury value couldn't be obtained but is required: we should fail suggesting that the node's version is too old
            (Just td, Just ctv) -> Just (ctv, td)

    -- We need to construct the txBodycontent outside of runTxBuild
    (balancedTxBody@(Exp.UnsignedTx tx), txBodyContent) <-
      fromExceptTCli $
        runTxBuild
          nodeSocketPath
          networkId
          mScriptValidity
          txinsAndMaybeScriptWits
          allReferenceInputs
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

        let mTxProtocolParams = Exp.txProtocolParams txBodyContent

        pparams <-
          mTxProtocolParams & fromMaybeCli TxCmdProtocolParametersNotPresentInTxBody
        let executionUnitPrices :: L.Prices = obtainCommonConstraints (Exp.useEra @era) $ pparams ^. L.ppPricesL

        Refl <-
          testEquality era' nodeEra
            & fromMaybeCli (NodeEraMismatchError era' nodeEra)

        let ledgerUTxO =
              obtainCommonConstraints (Exp.useEra @era) $ Api.toLedgerUTxO (convert $ Exp.useEra @era) txEraUtxo
            scriptExecUnitsMap =
              Exp.evaluateTransactionExecutionUnits
                systemStart
                (toLedgerEpochInfo eraHistory)
                pparams
                (obtainCommonConstraints (Exp.useEra @era) ledgerUTxO)
                tx

        let scriptHashes = Exp.collectPlutusScriptHashes balancedTxBody ledgerUTxO

        scriptCostOutput <-
          fromEitherCli $
            renderScriptCostsWithScriptHashesMap
              executionUnitPrices
              scriptHashes
              scriptExecUnitsMap
        liftIO $ LBS.writeFile (unFile fp) $ encodePretty scriptCostOutput
      OutputTxBodyOnly fpath -> fromEitherIOCli $ do
        let noWitTx = ShelleyTx (convert eon) $ obtainCommonConstraints (Exp.useEra @era) tx

        if isCborOutCanonical == TxCborCanonical
          then writeTxFileTextEnvelopeCanonical eon fpath noWitTx
          else writeTxFileTextEnvelope eon fpath noWitTx

runTransactionBuildEstimateCmd
  :: forall era e
   . Exp.IsEra era
  => Cmd.TransactionBuildEstimateCmdArgs era
  -> CIO e ()
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
    , voteFiles
    , proposalFiles
    , plutusCollateral
    , totalReferenceScriptSize
    , currentTreasuryValueAndDonation
    , isCborOutCanonical
    , txBodyOutFile
    } = do
    let sbe = convert currentEra

    ledgerPParams <-
      fromExceptTCli $
        readProtocolParameters @era protocolParamsFile

    txInsAndMaybeScriptWits <-
      readSpendScriptWitnesses txins

    certFilesAndMaybeScriptWits <-
      readCertificateScriptWitnesses @era certificates

    withdrawalsAndMaybeScriptWits <-
      mapM readWithdrawalScriptWitness withdrawals
    txMetadata <-
      readTxMetadata currentEra metadataSchema metadataFiles

    let (mas, sWitFiles) = fromMaybe mempty mMintedAssets
    valuesWithScriptWits <-
      (mas,) <$> mapM readMintScriptWitness sWitFiles

    scripts <-
      mapM (readFileScriptInAnyLang . unFile) scriptFiles
    txAuxScripts <-
      fromEitherCli $ validateTxAuxScripts scripts

    requiredSigners <-
      mapM (fromEitherIOCli . readRequiredSigner) reqSigners

    mReturnCollateral <- mapM toTxOutInShelleyBasedEra mReturnColl

    txOuts <- mapM (toTxOutInAnyEra sbe) txouts

    -- the same collateral input can be used for several plutus scripts
    let filteredTxinsc = nubOrd txInsCollateral

    -- Conway related
    votingProceduresAndMaybeScriptWits <-
      inEonForShelleyBasedEra
        (pure mempty)
        ( \w ->
            conwayEraOnwardsConstraints w $
              readVotingProceduresFiles voteFiles
        )
        sbe

    proposals <- readTxGovernanceActions proposalFiles

    certsAndMaybeScriptWits <-
      sequence $
        [ (,mSwit)
            <$> ( obtainCommonConstraints currentEra $
                    fromEitherIOCli $
                      readFileTextEnvelope (File certFile)
                )
        | (CertificateFile certFile, mSwit :: Exp.AnyWitness (Exp.LedgerEra era)) <-
            certFilesAndMaybeScriptWits
        ]

    txBodyContent <-
      fromEitherCli $
        constructTxBodyContent
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
          votingProceduresAndMaybeScriptWits
          proposals
          currentTreasuryValueAndDonation

    let stakeCredentialsToDeregisterMap = fromList $ catMaybes [getStakeDeregistrationInfo cert | (cert, _) <- certsAndMaybeScriptWits]
        drepsToDeregisterMap =
          fromList $
            catMaybes [getDRepDeregistrationInfo Exp.useEra cert | (cert, _) <- certsAndMaybeScriptWits]
        poolsToDeregister =
          fromList $
            catMaybes [getPoolDeregistrationInfo Exp.useEra cert | (cert, _) <- certsAndMaybeScriptWits]
        totCol = fromMaybe 0 plutusCollateral
        pScriptExecUnits =
          obtainCommonConstraints currentEra $
            fromList
              [ (obtainCommonConstraints currentEra index, Exp.getAnyPlutusScriptWitnessExecutionUnits psw)
              | (sWitIndex, Exp.AnyScriptWitnessPlutus psw) <-
                  Exp.collectTxBodyScriptWitnesses txBodyContent
              , index <- maybeToList $ Api.fromScriptWitnessIndex (convert currentEra) sWitIndex
              ]

    balancedTxBody :: Exp.TxBodyContent (Exp.LedgerEra era) <-
      fromEitherCli $
        first TxCmdFeeEstimationError $
          Exp.estimateBalancedTxBody
            currentEra
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
            (obtainCommonConstraints currentEra $ toLedgerValue (convert currentEra) totalUTxOValue)

    let unsignedTx = Exp.makeUnsignedTx currentEra balancedTxBody
    fromEitherIOCli
      $ ( if isCborOutCanonical == TxCborCanonical
            then writeTxFileTextEnvelopeCanonical
            else writeTxFileTextEnvelope
        )
        (convert currentEra)
        txBodyOutFile
      $ unsignedToToApiTx unsignedTx

unsignedToToApiTx :: forall era. Exp.IsEra era => Exp.UnsignedTx era -> Api.Tx era
unsignedToToApiTx (Exp.UnsignedTx lTx) =
  ShelleyTx (convert $ Exp.useEra @era) $ obtainCommonConstraints (Exp.useEra @era) lTx

fromShelleyLedgerPParamsShim
  :: Exp.Era era -> L.PParams (ShelleyLedgerEra era) -> L.PParams (Exp.LedgerEra era)
fromShelleyLedgerPParamsShim Exp.ConwayEra pp = pp
fromShelleyLedgerPParamsShim Exp.DijkstraEra pp = pp

getPoolDeregistrationInfo
  :: Exp.Era era
  -> Exp.Certificate (Exp.LedgerEra era)
  -> Maybe PoolId
getPoolDeregistrationInfo era (Exp.Certificate cert) =
  StakePoolKeyHash . fst
    <$> (obtainCommonConstraints era L.getRetirePoolTxCert cert :: Maybe (L.KeyHash L.StakePool, EpochNo))

getDRepDeregistrationInfo
  :: Exp.Era era
  -> Exp.Certificate (Exp.LedgerEra era)
  -> Maybe (L.Credential L.DRepRole, Lovelace)
getDRepDeregistrationInfo e (Exp.Certificate cert) =
  obtainCommonConstraints e $ L.getUnRegDRepTxCert cert

getStakeDeregistrationInfo
  :: forall era
   . Exp.IsEra era
  => Exp.Certificate (Exp.LedgerEra era)
  -> Maybe (StakeCredential, Lovelace)
getStakeDeregistrationInfo (Exp.Certificate cert) =
  getConwayDeregistrationInfo Exp.useEra cert

getConwayDeregistrationInfo
  :: forall era
   . Exp.Era era
  -> L.TxCert (Exp.LedgerEra era)
  -> Maybe (StakeCredential, Lovelace)
getConwayDeregistrationInfo e cert = do
  (stakeCred, depositRefund) <- obtainCommonConstraints e $ L.getUnRegDepositTxCert cert
  return (fromShelleyStakeCredential stakeCred, depositRefund)

runTransactionBuildRawCmd
  :: forall era e
   . Cmd.TransactionBuildRawCmdArgs era
  -> CIO e ()
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
    } = Exp.obtainCommonConstraints eon $ do
    txInsAndMaybeScriptWits <-
      readSpendScriptWitnesses txIns

    certFilesAndMaybeScriptWits :: [(CertificateFile, Exp.AnyWitness (Exp.LedgerEra era))] <-
      readCertificateScriptWitnesses certificates

    withdrawalsAndMaybeScriptWits <-
      mapM readWithdrawalScriptWitness withdrawals
    txMetadata <-
      readTxMetadata (convert Exp.useEra) metadataSchema metadataFiles

    let (mas, sWitFiles) = fromMaybe mempty mMintedAssets
    valuesWithScriptWits <-
      (mas,)
        <$> mapM readMintScriptWitness sWitFiles

    scripts <-
      mapM (readFileScriptInAnyLang . unFile) scriptFiles
    txAuxScripts <-
      fromEitherCli $
        validateTxAuxScripts scripts

    pparams <- forM mProtocolParamsFile $ \ppf ->
      fromExceptTCli (readProtocolParameters ppf)

    let mLedgerPParams = LedgerProtocolParameters <$> pparams

    -- TODO: Remove me as update proposals are deprecated since Conway (replaced with proposals)
    _txUpdateProposal <- case mUpdateProprosalFile of
      Just (Featured w (Just updateProposalFile)) ->
        fromExceptTCli $ readTxUpdateProposal w updateProposalFile
      _ -> pure TxUpdateProposalNone

    requiredSigners <-
      mapM (fromEitherIOCli . readRequiredSigner) reqSigners

    mReturnCollateral <- mapM toTxOutInShelleyBasedEra mReturnColl

    txOuts <- mapM (toTxOutInAnyEra (convert Exp.useEra)) txouts

    -- the same collateral input can be used for several plutus scripts
    let filteredTxinsc = toList @(Set _) $ fromList txInsCollateral

    -- Conway related
    votingProceduresAndMaybeScriptWits <-
      conwayEraOnwardsConstraints (convert $ Exp.useEra @era) $
        readVotingProceduresFiles voteFiles

    proposals <-
      readTxGovernanceActions @era proposalFiles

    certsAndMaybeScriptWits <-
      sequence
        [ (,mSwit)
            <$> ( obtainCommonConstraints eon $
                    fromEitherIOCli $
                      readFileTextEnvelope (File certFile)
                )
        | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
        ]
    txBody :: Exp.UnsignedTx era <-
      fromEitherCli $
        runTxBuildRaw
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
          votingProceduresAndMaybeScriptWits
          proposals
          currentTreasuryValueAndDonation

    let Exp.UnsignedTx lTx = txBody
        noWitTx = ShelleyTx (convert eon) lTx
    fromEitherIOCli $
      if isCborOutCanonical == TxCborCanonical
        then writeTxFileTextEnvelopeCanonical (convert Exp.useEra) txBodyOutFile noWitTx
        else writeTxFileTextEnvelope (convert Exp.useEra) txBodyOutFile noWitTx

runTxBuildRaw
  :: Exp.IsEra era
  => Maybe ScriptValidity
  -- ^ Mark script as expected to pass or fail validation
  -> [(TxIn, Exp.AnyWitness (Exp.LedgerEra era))]
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
  -> (L.MultiAsset, [(PolicyId, Exp.AnyWitness (Exp.LedgerEra era))])
  -- ^ Multi-Asset minted value(s)
  -> [(Exp.Certificate (Exp.LedgerEra era), Exp.AnyWitness (Exp.LedgerEra era))]
  -- ^ Certificate with potential script witness
  -> [(StakeAddress, Lovelace, Exp.AnyWitness (Exp.LedgerEra era))]
  -> [Hash PaymentKey]
  -- ^ Required signers
  -> TxAuxScripts era
  -> TxMetadataInEra era
  -> Maybe (LedgerProtocolParameters era)
  -> [(VotingProcedures era, Exp.AnyWitness (Exp.LedgerEra era))]
  -> [(Proposal era, Exp.AnyWitness (Exp.LedgerEra era))]
  -> Maybe (TxCurrentTreasuryValue, TxTreasuryDonation)
  -> Either TxCmdError (Exp.UnsignedTx era)
runTxBuildRaw
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
  votingProcedures
  proposals
  mCurrentTreasuryValueAndDonation = do
    txBodyContent <-
      constructTxBodyContent
        mScriptValidity
        (fromShelleyLedgerPParamsShim Exp.useEra . unLedgerProtocolParameters <$> mpparams)
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
        votingProcedures
        proposals
        mCurrentTreasuryValueAndDonation

    return $ Exp.makeUnsignedTx Exp.useEra txBodyContent

constructTxBodyContent
  :: forall era
   . Exp.IsEra era
  => Maybe ScriptValidity
  -> Maybe (L.PParams (Exp.LedgerEra era))
  -> [(TxIn, Exp.AnyWitness (Exp.LedgerEra era))]
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
  -> (L.MultiAsset, [(PolicyId, Exp.AnyWitness (Exp.LedgerEra era))])
  -- ^ Multi-Asset value(s)
  -> [(Exp.Certificate (Exp.LedgerEra era), Exp.AnyWitness (Exp.LedgerEra era))]
  -- ^ Certificate with potential script witness
  -> [(StakeAddress, Lovelace, Exp.AnyWitness (Exp.LedgerEra era))]
  -- ^ Withdrawals
  -> [Hash PaymentKey]
  -- ^ Required signers
  -> Lovelace
  -- ^ Tx fee
  -> TxAuxScripts era
  -> TxMetadataInEra era
  -> [(VotingProcedures era, Exp.AnyWitness (Exp.LedgerEra era))]
  -> [(Proposal era, Exp.AnyWitness (Exp.LedgerEra era))]
  -> Maybe (TxCurrentTreasuryValue, TxTreasuryDonation)
  -- ^ The current treasury value and the donation. This is a stop gap as the
  -- semantics of the donation and treasury value depend on the script languages
  -- being used.
  -> Either TxCmdError (Exp.TxBodyContent (Exp.LedgerEra era))
constructTxBodyContent
  mScriptValidity
  mPparams
  inputsAndMaybeScriptWits
  readOnlyRefIns
  txinsc
  mReturnCollateral
  mTotCollateral
  txouts
  mLowerBound
  (TxValidityUpperBound _ mUpperBound)
  valuesWithScriptWits
  certsAndMaybeScriptWits
  withdrawals
  reqSigners
  fee
  txAuxScripts
  txMetadata
  votingProcedures
  proposals
  mCurrentTreasuryValueAndDonation =
    do
      let allReferenceInputs =
            getAllReferenceInputs
              (map snd inputsAndMaybeScriptWits)
              (map snd $ snd valuesWithScriptWits)
              (map snd certsAndMaybeScriptWits)
              (map (\(_, _, mSwit) -> mSwit) withdrawals)
              (map snd votingProcedures)
              (map snd proposals)
              readOnlyRefIns
      -- TODO The last argument of validateTxInsReference is a datum set from reference inputs
      -- Should we allow providing of datum from CLI?
      -- TODO: Figure how to expose resolved datums
      let refInputs = Exp.TxInsReference allReferenceInputs Set.empty
          expTxouts = map Exp.fromLegacyTxOut txouts
          auxScripts = case txAuxScripts of
            TxAuxScriptsNone -> []
            -- TODO: Auxiliary scripts cannot be plutus scripts
            TxAuxScripts _ scripts -> mapMaybe scriptInEraToSimpleScript scripts
          txRetCollateral = case mReturnCollateral of
            Just rc ->
              let Exp.TxOut o _ = Exp.fromLegacyTxOut rc
               in Just $ Exp.TxReturnCollateral (o :: (L.TxOut (Exp.LedgerEra era)))
            Nothing -> Nothing
          txTotCollateral = Exp.TxTotalCollateral <$> (mTotCollateral :: Maybe L.Coin)
          expTxMetadata = case txMetadata of
            TxMetadataNone -> TxMetadata mempty
            TxMetadataInEra _ mDat -> mDat

      validatedMintValue <- createTxMintValue valuesWithScriptWits
      let vProcedures = convertVotingProcedures votingProcedures
      validatedVotingProcedures <-
        first (TxCmdTxGovDuplicateVotes . TxGovDuplicateVotes) $
          Exp.mkTxVotingProcedures vProcedures
      let txProposals = [(obtainCommonConstraints (Exp.useEra @era) p, w) | (Proposal p, w) <- proposals]
      let validatedTxProposals =
            Exp.mkTxProposalProcedures txProposals
      let validatedCurrentTreasuryValue = unTxCurrentTreasuryValue . fst <$> mCurrentTreasuryValueAndDonation
          validatedTreasuryDonation = unTxTreasuryDonation . snd <$> mCurrentTreasuryValueAndDonation
      let validatedWithdrawals = convertWithdrawals withdrawals
      return
        ( Exp.defaultTxBodyContent
            & Exp.setTxIns inputsAndMaybeScriptWits
            & Exp.setTxInsCollateral txinsc
            & Exp.setTxInsReference refInputs
            & Exp.setTxOuts expTxouts
            & maybe id Exp.setTxReturnCollateral txRetCollateral
            & maybe id Exp.setTxTotalCollateral txTotCollateral
            & Exp.setTxFee fee
            & maybe id Exp.setTxValidityLowerBound mLowerBound
            & maybe id Exp.setTxValidityUpperBound mUpperBound
            & Exp.setTxMetadata expTxMetadata
            & Exp.setTxAuxScripts auxScripts
            & Exp.setTxWithdrawals validatedWithdrawals
            & Exp.setTxExtraKeyWits (Exp.TxExtraKeyWitnesses reqSigners)
            & maybe id (Exp.setTxProtocolParams . Exp.obtainCommonConstraints (Exp.useEra @era)) mPparams
            & Exp.setTxCertificates
              (Exp.mkTxCertificates Exp.useEra certsAndMaybeScriptWits)
            & Exp.setTxMintValue validatedMintValue
            & Exp.setTxScriptValidity (fromMaybe ScriptValid mScriptValidity)
            & Exp.setTxVotingProcedures validatedVotingProcedures
            & Exp.setTxProposalProcedures validatedTxProposals
            & maybe id Exp.setTxCurrentTreasuryValue validatedCurrentTreasuryValue
            & maybe id Exp.setTxTreasuryDonation validatedTreasuryDonation
        )

convertWithdrawals
  :: [(StakeAddress, L.Coin, Exp.AnyWitness (Exp.LedgerEra era))]
  -> Exp.TxWithdrawals (Exp.LedgerEra era)
convertWithdrawals = Exp.TxWithdrawals

convertVotingProcedures
  :: forall era
   . Exp.IsEra era
  => [(VotingProcedures era, Exp.AnyWitness (Exp.LedgerEra era))]
  -> [(L.VotingProcedures (Exp.LedgerEra era), Exp.AnyWitness (Exp.LedgerEra era))]
convertVotingProcedures =
  map
    ( \(VotingProcedures vp, wit) ->
        (obtainCommonConstraints (Exp.useEra @era) vp, wit)
    )

scriptInEraToSimpleScript
  :: forall era. Exp.IsEra era => ScriptInEra era -> Maybe (Exp.SimpleScript (Exp.LedgerEra era))
scriptInEraToSimpleScript s =
  obtainCommonConstraints (Exp.useEra @era) $
    Exp.SimpleScript
      <$> L.getNativeScript (obtainCommonConstraints (Exp.useEra @era) $ toShelleyScript s)

runTxBuild
  :: forall era
   . Exp.IsEra era
  => HasCallStack
  => SocketPath
  -> NetworkId
  -> Maybe ScriptValidity
  -- ^ Mark script as expected to pass or fail validation
  -> [(TxIn, Exp.AnyWitness (Exp.LedgerEra era))]
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
  -> (L.MultiAsset, [(PolicyId, Exp.AnyWitness (Exp.LedgerEra era))]) -- TODO: Double check why this is a list

  -- ^ Multi-Asset value(s)
  -> Maybe SlotNo
  -- ^ Tx lower bound
  -> TxValidityUpperBound era
  -- ^ Tx upper bound
  -> [(Exp.Certificate (Exp.LedgerEra era), Exp.AnyWitness (Exp.LedgerEra era))]
  -- ^ Certificate with potential script witness
  -> [(StakeAddress, Lovelace, Exp.AnyWitness (Exp.LedgerEra era))]
  -> [Hash PaymentKey]
  -- ^ Required signers
  -> TxAuxScripts era
  -> TxMetadataInEra era
  -> TxUpdateProposal era
  -> Maybe Word
  -> [(VotingProcedures era, Exp.AnyWitness (Exp.LedgerEra era))]
  -> [(Proposal era, Exp.AnyWitness (Exp.LedgerEra era))]
  -> Maybe (TxCurrentTreasuryValue, TxTreasuryDonation)
  -- ^ The current treasury value and the donation.
  -> ExceptT TxCmdError IO (Exp.UnsignedTx era, Exp.TxBodyContent (Exp.LedgerEra era))
runTxBuild
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
  _txUpdateProposal -- TODO: Remove this parameter
  mOverrideWits
  votingProcedures
  proposals
  mCurrentTreasuryValueAndDonation = do
    let sbe = convert (Exp.useEra @era)
    shelleyBasedEraConstraints sbe $ do
      -- TODO: All functions should be parameterized by ShelleyBasedEra
      -- as it's not possible to call this function with ByronEra
      let era = toCardanoEra sbe
          inputsThatRequireWitnessing = [input | (input, _) <- inputsAndMaybeScriptWits]
      let allReferenceInputs =
            getAllReferenceInputs
              (map snd inputsAndMaybeScriptWits)
              (map snd $ snd mintValueWithScriptWits)
              (map snd certsAndMaybeScriptWits)
              (map (\(_, _, wit) -> wit) withdrawals)
              (map snd votingProcedures)
              (map snd proposals)
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

      let certsToQuery = obtainCommonConstraints (Exp.useEra @era) (fst <$> certsAndMaybeScriptWits)
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
            mScriptValidity
            (Just $ fromShelleyLedgerPParamsShim Exp.useEra $ unLedgerProtocolParameters pparams)
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
            votingProcedures
            proposals
            mCurrentTreasuryValueAndDonation

      firstExceptT TxCmdTxInsDoNotExist
        . hoistEither
        $ txInsExistInUTxO allTxInputs txEraUtxo
      firstExceptT TxCmdQueryNotScriptLocked
        . hoistEither
        $ notScriptLockedTxIns txinsc txEraUtxo
      let ledgerUTxO = Api.toLedgerUTxO (convert Exp.useEra) txEraUtxo
      cAddr <-
        pure (anyAddressInEra era changeAddr)
          & onLeft (error $ "runTxBuild: Byron address used: " <> show changeAddr) -- should this throw instead?
      r@(unsignedTx, _) <-
        firstExceptT (TxCmdBalanceTxBody . AnyTxBodyErrorAutoBalance)
          . hoistEither
          $ Exp.makeTransactionBodyAutoBalance
            systemStart
            (toLedgerEpochInfo eraHistory)
            (Exp.obtainCommonConstraints (Exp.useEra @era) $ unLedgerProtocolParameters pparams)
            stakePools
            stakeDelegDeposits
            (Map.map L.fromCompact drepDelegDeposits)
            (obtainCommonConstraints (Exp.useEra @era) ledgerUTxO)
            txBodyContent
            cAddr
            mOverrideWits
      -- Check to see if we lost any scripts during balancing
      scriptWitnessesBeforeBalance <-
        firstExceptT TxCmdCBORDecodeError $
          hoistEither $
            Exp.extractAllIndexedPlutusScriptWitnesses Exp.useEra txBodyContent
      scriptWitnessesAfterBalance <-
        hoistEither . first TxCmdCBORDecodeError $
          Exp.extractAllIndexedPlutusScriptWitnesses Exp.useEra (snd r)
      when
        ( length scriptWitnessesBeforeBalance
            /= length scriptWitnessesAfterBalance
        )
        $ left
        $ LostScriptWitnesses scriptWitnessesBeforeBalance scriptWitnessesAfterBalance

      liftIO . putStrLn . docToString $
        "Estimated transaction fee:" <+> pretty (Exp.getUnsignedTxFee unsignedTx)

      return r

-- ----------------------------------------------------------------------------
-- Transaction body validation and conversion
--

getAllReferenceInputs
  :: [Exp.AnyWitness (Exp.LedgerEra era)]
  -> [Exp.AnyWitness (Exp.LedgerEra era)]
  -> [Exp.AnyWitness (Exp.LedgerEra era)]
  -- \^ Certificate witnesses
  -> [Exp.AnyWitness (Exp.LedgerEra era)]
  -> [Exp.AnyWitness (Exp.LedgerEra era)]
  -> [Exp.AnyWitness (Exp.LedgerEra era)]
  -> [TxIn]
  -- \^ Read only reference inputs
  -> [TxIn]
getAllReferenceInputs
  spendingWitnesses
  mintWitnesses
  certScriptWitnesses
  withdrawals
  votingProceduresAndMaybeScriptWits
  propProceduresAnMaybeScriptWits
  readOnlyRefIns = do
    let txinsWitByRefInputs = mapMaybe Exp.getAnyWitnessReferenceInput spendingWitnesses
        mintingRefInputs = mapMaybe Exp.getAnyWitnessReferenceInput mintWitnesses
        certsWitByRefInputs = mapMaybe Exp.getAnyWitnessReferenceInput certScriptWitnesses
        withdrawalsWitByRefInputs = mapMaybe Exp.getAnyWitnessReferenceInput withdrawals
        votesWitByRefInputs = mapMaybe Exp.getAnyWitnessReferenceInput votingProceduresAndMaybeScriptWits
        propsWitByRefInputs = mapMaybe Exp.getAnyWitnessReferenceInput propProceduresAnMaybeScriptWits

    concat
      [ txinsWitByRefInputs
      , mintingRefInputs
      , certsWitByRefInputs
      , withdrawalsWitByRefInputs
      , votesWitByRefInputs
      , propsWitByRefInputs
      , mapMaybe Just readOnlyRefIns
      ]

toTxOutInShelleyBasedEra
  :: Exp.IsEra era
  => TxOutShelleyBasedEra
  -> CIO e (TxOut CtxTx era)
toTxOutInShelleyBasedEra (TxOutShelleyBasedEra addr' val' mDatumHash refScriptFp) = do
  let sbe = convert Exp.useEra
      addr = shelleyAddressInEra sbe addr'
  mkTxOut sbe addr val' mDatumHash refScriptFp

-- TODO: Currently we specify the policyId with the '--mint' option on the cli
-- and we added a separate '--policy-id' parser that parses the policy id for the
-- given reference input (since we don't have the script in this case). To avoid asking
-- for the policy id twice (in the build command) we can potentially query the UTxO and
-- access the script (and therefore the policy id).
createTxMintValue
  :: (L.MultiAsset, [(PolicyId, Exp.AnyWitness (Exp.LedgerEra era))])
  -> Either TxCmdError (Exp.TxMintValue (Exp.LedgerEra era))
createTxMintValue (val, scriptWitnesses) =
  if mempty == val && List.null scriptWitnesses
    then return $ Exp.TxMintValue Map.empty
    else do
      let policiesWithAssets :: Map PolicyId PolicyAssets
          policiesWithAssets = multiAssetToPolicyAssets val
          -- The set of policy ids for which we need witnesses:
          witnessesNeededSet :: Set PolicyId
          witnessesNeededSet = Map.keysSet policiesWithAssets

      let witnessesProvidedMap = fromList scriptWitnesses
          witnessesProvidedSet :: Set PolicyId
          witnessesProvidedSet = Map.keysSet witnessesProvidedMap

      -- Check not too many, nor too few:
      validateAllWitnessesProvided witnessesNeededSet witnessesProvidedSet
      validateNoUnnecessaryWitnesses witnessesNeededSet witnessesProvidedSet
      pure $
        Exp.TxMintValue $
          Map.intersectionWith
            (\assets wit -> (assets, wit))
            policiesWithAssets
            witnessesProvidedMap
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
        anyTx <- lift (readFileTx inputTxFile) & onLeft (left . TxCmdTextEnvError)

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
              then writeTxFileTextEnvelopeCanonical sbe outTxFile signedTx
              else writeTxFileTextEnvelope sbe outTxFile signedTx
      InputTxBodyFile (File txbodyFilePath) -> do
        txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
        unwitnessed <-
          firstExceptT TxCmdTextEnvError . newExceptT $
            readFileTxBody txbodyFile

        case unwitnessed of
          IncompleteTxBody anyTxBody -> do
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
                  then writeTxFileTextEnvelopeCanonical sbe outTxFile tx
                  else writeTxFileTextEnvelope sbe outTxFile tx

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
      lift (readFileTx txFileOrPipe) & onLeft (left . TxCmdTextEnvError)
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
  -> CIO e ()
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
      fromEitherIOCli $
        readFileTxBody txbodyFile

    let nShelleyKeyWitW32 = fromIntegral nShelleyKeyWitnesses

    InAnyShelleyBasedEra sbe txbody <- pure $ unIncompleteTxBody unwitnessed

    era <- fromEitherCli $ Exp.sbeToEra sbe
    lpparams <-
      fromExceptTCli @ProtocolParamsError $
        Exp.obtainCommonConstraints era $
          readProtocolParameters protocolParamsFile

    let shelleyfee = evaluateTransactionFee sbe lpparams txbody nShelleyKeyWitW32 0 sReferenceScript

    let byronfee =
          shelleyBasedEraConstraints sbe $
            calculateByronWitnessFees (lpparams ^. L.ppMinFeeAL) nByronKeyWitnesses

    let fee = shelleyfee + byronfee
        textToWrite = docToText $ pretty fee
        content = Aeson.object ["fee" .= fee]

    outputFormat
      & ( id
            . Vary.on
              ( \FormatJson -> case outFile of
                  Nothing ->
                    liftIO $ LBS.putStrLn $ Json.encodeJson content
                  Just file ->
                    fromEitherIOCli @(FileError ()) $
                      writeLazyByteStringFile file $
                        Json.encodeJson content
              )
            . Vary.on
              ( \FormatText -> case outFile of
                  Nothing ->
                    liftIO $ Text.putStrLn textToWrite
                  Just file ->
                    fromEitherIOCli @(FileError ()) $ writeTextFile file textToWrite
              )
            . Vary.on
              ( \FormatYaml -> case outFile of
                  Nothing ->
                    liftIO $ LBS.putStrLn $ Json.encodeYaml content
                  Just file ->
                    fromEitherIOCli @(FileError ()) $
                      writeLazyByteStringFile file $
                        Json.encodeYaml content
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
  -> CIO e ()
runTransactionCalculateMinValueCmd
  Cmd.TransactionCalculateMinValueCmdArgs
    { era
    , protocolParamsFile
    , txOut
    } = do
    pp <-
      fromExceptTCli @ProtocolParamsError
        (obtainCommonConstraints era $ readProtocolParameters protocolParamsFile)
    out <- obtainCommonConstraints era $ toTxOutInShelleyBasedEra txOut

    let minValue = calculateMinimumUTxO (convert era) pp out
    liftIO . IO.print $ minValue

runTransactionCalculatePlutusScriptCostCmd
  :: Cmd.TransactionCalculatePlutusScriptCostCmdArgs era -> ExceptT TxCmdError IO ()
runTransactionCalculatePlutusScriptCostCmd
  Cmd.TransactionCalculatePlutusScriptCostCmdArgs
    { nodeContextInfoSource
    , txFileIn
    , outputFile
    } = do
    txFileOrPipeIn <- liftIO $ fileOrPipe txFileIn
    InAnyShelleyBasedEra txEra tx@(ShelleyTx sbe ledgerTx) <-
      liftIO (readFileTx txFileOrPipeIn) & onLeft (left . TxCmdTextEnvError)

    let relevantTxIns :: Set TxIn
        relevantTxIns = Set.map fromShelleyTxIn $ shelleyBasedEraConstraints sbe (ledgerTx ^. bodyTxL . allInputsTxBodyF)

    (AnyCardanoEra nodeEra, systemStart, eraHistory, txEraUtxo, pparams) <-
      case nodeContextInfoSource of
        NodeConnectionInfo nodeConnInfo ->
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
        ProvidedTransactionContextInfo
          ( TransactionContext
              { systemStartSource
              , mustExtendSafeZone
              , eraHistoryFile
              , utxoFile
              , protocolParamsFile
              }
            ) -> do
            era <- hoistEither $ first TxCmdDeprecatedEra $ Exp.sbeToEra sbe
            buildTransactionContext
              era
              systemStartSource
              mustExtendSafeZone
              eraHistoryFile
              utxoFile
              protocolParamsFile

    Refl <-
      testEquality nodeEra (convert txEra)
        & hoistMaybe
          ( TxCmdTxSubmitErrorEraMismatch $
              EraMismatch{ledgerEraName = docToText $ pretty nodeEra, otherEraName = docToText $ pretty txEra}
          )

    aeo <- forEraMaybeEon nodeEra & hoistMaybe (TxCmdAlonzoEraOnwardsRequired nodeEra)
    calculatePlutusScriptsCosts aeo systemStart eraHistory pparams txEraUtxo tx
   where
    calculatePlutusScriptsCosts
      :: AlonzoEraOnwards era
      -> SystemStart
      -> EraHistory
      -> LedgerProtocolParameters era
      -> UTxO era
      -> Tx era
      -> ExceptT TxCmdError IO ()
    calculatePlutusScriptsCosts aeo systemStart eraHistory pparams txEraUtxo tx = do
      let era' = toCardanoEra aeo

      let scriptHashes = collectPlutusScriptHashes aeo tx txEraUtxo

      executionUnitPrices <-
        pure (getExecutionUnitPrices era' pparams) & onNothing (left TxCmdPParamExecutionUnitsNotAvailable)

      let scriptExecUnitsMap =
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

buildTransactionContext
  :: Exp.Era era
  -> SystemStartOrGenesisFileSource
  -> MustExtendSafeZone
  -> File EraHistory In
  -> File (Some UTxO) In
  -> ProtocolParamsFile
  -> ExceptT
       TxCmdError
       IO
       (AnyCardanoEra, SystemStart, EraHistory, UTxO era, LedgerProtocolParameters era)
buildTransactionContext era systemStartOrGenesisFileSource mustUnsafeExtendSafeZone eraHistoryFile utxoFile protocolParamsFile =
  shelleyBasedEraConstraints (convert era) $ do
    ledgerPParams <-
      firstExceptT TxCmdProtocolParamsError $
        obtainCommonConstraints era $
          readProtocolParameters protocolParamsFile
    EraHistory interpreter <-
      onLeft (left . TxCmdTextEnvError) $
        liftIO $
          readFileTextEnvelope eraHistoryFile
    systemStart <- case systemStartOrGenesisFileSource of
      SystemStartLiteral systemStart -> return systemStart
      SystemStartFromGenesisFile (GenesisFile byronGenesisFile) -> do
        (byronGenesisData, _) <- firstExceptT TxCmdGenesisDataError $ Byron.readGenesisData byronGenesisFile
        let systemStartUTCTime = Byron.gdStartTime byronGenesisData
        return $ SystemStart systemStartUTCTime
    utxosBytes <- modifyError TxCmdUtxoFileError (ExceptT $ readByteStringFile utxoFile)
    utxos <- liftEither . first TxCmdUtxoJsonError $ Aeson.eitherDecodeStrict' utxosBytes
    let eraHistory = EraHistory $ case mustUnsafeExtendSafeZone of
          MustExtendSafeZone -> unsafeExtendSafeZone interpreter
          DoNotExtendSafeZone -> interpreter
    return
      ( AnyCardanoEra (convert era)
      , systemStart
      , eraHistory
      , utxos
      , LedgerProtocolParameters ledgerPParams
      )

runTransactionPolicyIdCmd
  :: Cmd.TransactionPolicyIdCmdArgs
  -> CIO e ()
runTransactionPolicyIdCmd
  Cmd.TransactionPolicyIdCmdArgs
    { scriptFile = File sFile
    } = do
    script <-
      readAnyScript @_ @ConwayEra sFile
    let hash = fromShelleyScriptHash $ Exp.hashAnyScript script
    liftIO . Text.putStrLn $ serialiseToRawBytesHexText hash

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
            firstExceptT TxCmdTextEnvError . newExceptT $
              readFileTxBody txbodyFile
          return $ unIncompleteTxBody unwitnessed
        InputTxFile (File txFilePath) -> do
          txFile <- liftIO $ fileOrPipe txFilePath
          InAnyShelleyBasedEra era tx <- lift (readFileTx txFile) & onLeft (left . TxCmdTextEnvError)
          return . InAnyShelleyBasedEra era $ getTxBody tx

    let txId = getTxId txbody

    liftIO $
      outputFormat
        & ( id
              . Vary.on (\FormatJson -> LBS.putStrLn $ Json.encodeJson $ TxSubmissionResult txId)
              . Vary.on (\FormatText -> BS.putStrLn $ serialiseToRawBytesHex txId)
              . Vary.on (\FormatYaml -> LBS.putStrLn $ Json.encodeYaml $ TxSubmissionResult txId)
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
      firstExceptT TxCmdTextEnvError . newExceptT $
        readFileTxBody txbodyFile
    case unwitnessed of
      IncompleteTxBody anyTxBody -> do
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
          writeTxWitnessFileTextEnvelope sbe outFile witness

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
    IncompleteTxBody (InAnyShelleyBasedEra era txbody) <-
      lift (readFileTxBody txbodyFile) & onLeft (left . TxCmdTextEnvError)
    witnesses <-
      sequence
        [ do
            InAnyShelleyBasedEra era' witness <-
              lift (readFileTxKeyWitness file)
                & onLeft (left . TxCmdTextEnvError)

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
          then writeTxFileTextEnvelopeCanonical era outFile tx
          else writeTxFileTextEnvelope era outFile tx

getExecutionUnitPrices :: CardanoEra era -> LedgerProtocolParameters era -> Maybe L.Prices
getExecutionUnitPrices cEra (LedgerProtocolParameters pp) =
  forEraInEonMaybe cEra $ \aeo ->
    alonzoEraOnwardsConstraints aeo $
      pp ^. L.ppPricesL
