{-# LANGUAGE AllowAmbiguousTypes #-}
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

-- | Runners for Dijkstra sub-transactions: the @transaction sub-transaction@
-- command group.
module Cardano.CLI.EraBased.Transaction.SubTransaction.Run
  ( runSubTransactionCmds
  , runSubTransactionBuildRawCmd
  , runSubTransactionSignCmd
  , runSubTransactionTxIdCmd
  )
where

import Cardano.Api
import Cardano.Api.Experimental (obtainCommonConstraints)
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.AnyScriptWitness qualified as Exp
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Genesis.Internal.Common (readProtocolParameters)
import Cardano.CLI.EraBased.Script.Certificate.Read
import Cardano.CLI.EraBased.Script.Mint.Read
import Cardano.CLI.EraBased.Script.Proposal.Read
import Cardano.CLI.EraBased.Script.Spend.Read
import Cardano.CLI.EraBased.Script.Vote.Read
import Cardano.CLI.EraBased.Script.Withdrawal.Read
import Cardano.CLI.EraBased.Transaction.Internal.Common
import Cardano.CLI.EraBased.Transaction.SubTransaction.Command qualified as Cmd
import Cardano.CLI.Json.Encode qualified as Json
import Cardano.CLI.Read
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.TxCmdError
import Cardano.CLI.Type.Error.TxValidationError
import Cardano.CLI.Type.Key (readVerificationKeyOrHashOrFileOrScriptHash)
import Cardano.Ledger.Hashes (DataHash)
import Cardano.Ledger.Keys (coerceKeyRole)

import RIO hiding (toList)

import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Map.Strict qualified as Map
import Data.OSet.Strict (OSet)
import Data.OSet.Strict qualified as OSet
import Data.Set qualified as Set
import Vary qualified

runSubTransactionCmds :: Cmd.SubTransactionCmds era -> CIO e ()
runSubTransactionCmds = \case
  Cmd.SubTransactionBuildRawCmd args -> runSubTransactionBuildRawCmd args
  Cmd.SubTransactionSignCmd args -> runSubTransactionSignCmd args
  Cmd.SubTransactionTxIdCmd args -> runSubTransactionTxIdCmd args

runSubTransactionBuildRawCmd
  :: forall era e
   . Cmd.SubTransactionBuildRawCmdArgs era
  -> CIO e ()
runSubTransactionBuildRawCmd
  Cmd.SubTransactionBuildRawCmdArgs
    { eon
    , txIns
    , readOnlyRefIns
    , txouts
    , mMintedAssets
    , mValidityLowerBound
    , mValidityUpperBound
    , certificates
    , withdrawals
    , metadataSchema
    , scriptFiles
    , metadataFiles
    , mProtocolParamsFile
    , voteFiles
    , proposalFiles
    , mCurrentTreasuryValue
    , mTreasuryDonation
    , guards
    , outFile
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

    txOutsAndDatums <- mapM toTxOutInEra txouts
    let txOuts = map fst txOutsAndDatums
        supplementalDatums = mconcat (map snd txOutsAndDatums)

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

    guardCredentials <-
      mapM (readVerificationKeyOrHashOrFileOrScriptHash (\(PaymentKeyHash kh) -> coerceKeyRole kh)) guards

    subTx <-
      fromEitherCli $
        constructSubTx
          pparams
          txInsAndMaybeScriptWits
          readOnlyRefIns
          txOuts
          mValidityLowerBound
          mValidityUpperBound
          valuesWithScriptWits
          certsAndMaybeScriptWits
          withdrawalsAndMaybeScriptWits
          txAuxScripts
          txMetadata
          votingProceduresAndMaybeScriptWits
          proposals
          mCurrentTreasuryValue
          mTreasuryDonation
          supplementalDatums
          (OSet.fromList guardCredentials)

    unsignedSubTx <-
      fromEitherCli $ first TxCmdMakeUnsignedTxError $ Exp.makeUnsignedSubTx eon subTx

    fromEitherIOCli $ writeFileTextEnvelope outFile Nothing unsignedSubTx

-- | The sub-transaction analogue of 'constructTxBodyContent'. It shares the
-- value preparation but ends in a 'Exp.SubTx' setter chain, which has no fee,
-- collateral, required signers or script validity, and adds guards.
constructSubTx
  :: forall era
   . Exp.IsEra era
  => Maybe (L.PParams (Exp.LedgerEra era))
  -> [(TxIn, Exp.AnyWitness (Exp.LedgerEra era))]
  -- ^ TxIn with potential script witness
  -> [TxIn]
  -- ^ Read only reference inputs
  -> [Exp.TxOut (Exp.LedgerEra era)]
  -> Maybe SlotNo
  -- ^ Lower bound
  -> Maybe SlotNo
  -- ^ Upper bound
  -> (L.MultiAsset, [(PolicyId, Exp.AnyScriptWitness (Exp.LedgerEra era))])
  -- ^ Multi-Asset value(s)
  -> [(Exp.Certificate (Exp.LedgerEra era), Exp.AnyWitness (Exp.LedgerEra era))]
  -- ^ Certificate with potential script witness
  -> [(StakeAddress, Lovelace, Exp.AnyWitness (Exp.LedgerEra era))]
  -- ^ Withdrawals
  -> TxAuxScripts era
  -> TxMetadataInEra era
  -> [(VotingProcedures era, Exp.AnyWitness (Exp.LedgerEra era))]
  -> [(Proposal era, Exp.AnyWitness (Exp.LedgerEra era))]
  -> Maybe TxCurrentTreasuryValue
  -> Maybe TxTreasuryDonation
  -> Map.Map DataHash (L.Data (Exp.LedgerEra era))
  -- ^ Supplemental datums
  -> OSet (L.Credential L.Guard)
  -- ^ Guards
  -> Either TxCmdError (Exp.SubTx (Exp.LedgerEra era))
constructSubTx
  mPparams
  inputsAndMaybeScriptWits
  readOnlyRefIns
  txouts
  mLowerBound
  mUpperBound
  valuesWithScriptWits
  certsAndMaybeScriptWits
  withdrawals
  txAuxScripts
  txMetadata
  votingProcedures
  proposals
  mCurrentTreasury
  mTreasuryDonation
  suppDatums
  guards =
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
          refInputs = Exp.TxInsReference allReferenceInputs Set.empty
          auxScripts = case txAuxScripts of
            TxAuxScriptsNone -> []
            TxAuxScripts _ scripts -> mapMaybe scriptInEraToSimpleScript scripts
          expTxMetadata = case txMetadata of
            TxMetadataNone -> TxMetadata mempty
            TxMetadataInEra _ mDat -> mDat

      validatedMintValue <- createTxMintValue valuesWithScriptWits
      validatedVotingProcedures <-
        first (TxCmdVotingError . TxVotingError) $
          Exp.mkTxVotingProcedures (convertVotingProcedures votingProcedures)
      let txProposals = [(obtainCommonConstraints (Exp.useEra @era) p, w) | (Proposal p, w) <- proposals]
      return
        ( Exp.defaultSubTx
            & Exp.setSubTxIns inputsAndMaybeScriptWits
            & Exp.setSubTxInsReference refInputs
            & Exp.setSubTxOuts txouts
            & maybe id Exp.setSubTxValidityLowerBound mLowerBound
            & maybe id Exp.setSubTxValidityUpperBound mUpperBound
            & Exp.setSubTxMetadata expTxMetadata
            & Exp.setSubTxAuxScripts auxScripts
            & Exp.setSubTxWithdrawals (convertWithdrawals withdrawals)
            & maybe id (Exp.setSubTxProtocolParams . Exp.obtainCommonConstraints (Exp.useEra @era)) mPparams
            & Exp.setSubTxCertificates
              (Exp.mkTxCertificates Exp.useEra certsAndMaybeScriptWits)
            & Exp.setSubTxMintValue validatedMintValue
            & Exp.setSubTxVotingProcedures validatedVotingProcedures
            & Exp.setSubTxProposalProcedures (Exp.mkTxProposalProcedures txProposals)
            & maybe id Exp.setSubTxCurrentTreasuryValue (unTxCurrentTreasuryValue <$> mCurrentTreasury)
            & maybe id Exp.setSubTxTreasuryDonation (unTxTreasuryDonation <$> mTreasuryDonation)
            & Exp.setSubTxSupplementalDatums suppDatums
            & Exp.setSubTxGuards guards
        )

runSubTransactionSignCmd
  :: Cmd.SubTransactionSignCmdArgs
  -> CIO e ()
runSubTransactionSignCmd
  Cmd.SubTransactionSignCmdArgs
    { subTxFile = File subTxFilePath
    , witnessSigningData
    , outFile
    } = do
    sks <- forM witnessSigningData $ \d ->
      fromEitherIOCli $ first TxCmdReadWitnessSigningDataError <$> readWitnessSigningData d

    let (sksByron, sksShelley) = partitionSomeWitnesses $ map categoriseSomeSigningWitness sks

    -- A bootstrap witness needs a top-level body to sign, see 'mkShelleyBootstrapWitness'.
    unless (null sksByron) $
      throwCliError TxCmdSubTxByronWitnessUnsupported

    subTxFile <- liftIO $ fileOrPipe subTxFilePath
    AnyUnsignedSubTx era unsigned <-
      fromEitherIOCli $ first TxCmdTextEnvError <$> readFileUnsignedSubTx subTxFile

    let keyWits = map (Exp.makeSubTxKeyWitness unsigned) sksShelley
        signed = Exp.signSubTx [] keyWits unsigned

    Exp.obtainCommonConstraints era $
      fromEitherIOCli $
        writeFileTextEnvelope outFile Nothing signed

runSubTransactionTxIdCmd
  :: Cmd.SubTransactionTxIdCmdArgs
  -> CIO e ()
runSubTransactionTxIdCmd
  Cmd.SubTransactionTxIdCmdArgs
    { inputSubTxFile
    , outputFormat
    } = do
    subTxId <-
      case inputSubTxFile of
        InputUnsignedSubTxFile (File path) -> do
          file <- liftIO $ fileOrPipe path
          AnyUnsignedSubTx _ unsigned <-
            fromEitherIOCli $ first TxCmdTextEnvError <$> readFileUnsignedSubTx file
          pure $ Exp.getUnsignedSubTxId unsigned
        InputSignedSubTxFile (File path) -> do
          file <- liftIO $ fileOrPipe path
          AnySignedSubTx _ signed <-
            fromEitherIOCli $ first TxCmdTextEnvError <$> readFileSignedSubTx file
          pure $ Exp.getSignedSubTxId signed

    liftIO $
      outputFormat
        & ( id
              . Vary.on (\FormatJson -> LBS.putStrLn $ Json.encodeJson $ TxSubmissionResult subTxId)
              . Vary.on (\FormatText -> BS.putStrLn $ serialiseToRawBytesHex subTxId)
              . Vary.on (\FormatYaml -> LBS.putStrLn $ Json.encodeYaml $ TxSubmissionResult subTxId)
              $ Vary.exhaustiveCase
          )
