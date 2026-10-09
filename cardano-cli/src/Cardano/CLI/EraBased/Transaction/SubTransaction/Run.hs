{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Runners for Dijkstra sub-transactions: the @transaction sub-transaction@
-- command group, and reading signed sub-transactions for embedding in a
-- top-level transaction.
module Cardano.CLI.EraBased.Transaction.SubTransaction.Run
  ( runSubTransactionCmds
  , runSubTransactionBuildRawCmd
  , runSubTransactionSignCmd
  , runSubTransactionTxIdCmd
  , readSignedSubTransactions
  , validateCanonicalSubTransactions
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
import Cardano.Ledger.Dijkstra.TxBody qualified as L
import Cardano.Ledger.Hashes (DataHash, originalBytes)
import Cardano.Ledger.Keys (coerceKeyRole)
import Cardano.Ledger.Plutus.Language qualified as L

import RIO hiding (toList)

import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.OSet.Strict (OSet)
import Data.OSet.Strict qualified as OSet
import Data.Set qualified as Set
import Vary qualified

runSubTransactionCmds :: Cmd.SubTransactionCmds -> CIO e ()
runSubTransactionCmds = \case
  Cmd.SubTransactionBuildRawCmd args -> runSubTransactionBuildRawCmd args
  Cmd.SubTransactionSignCmd args -> runSubTransactionSignCmd args
  Cmd.SubTransactionTxIdCmd args -> runSubTransactionTxIdCmd args

-- | Read the signed sub-transactions named on the command line, checking that
-- none is given twice (the ledger keys them by id, so a duplicate would
-- silently collapse).
readSignedSubTransactions
  :: [SignedSubTxFile In]
  -> CIO e [Exp.SignedSubTx]
readSignedSubTransactions subTransactionFiles = do
  signedSubTxs <- forM subTransactionFiles $ \(File subTxPath) -> do
    subTxFile <- liftIO $ fileOrPipe subTxPath
    fromEitherIOCli $ first TxCmdTextEnvError <$> readFileSignedSubTx subTxFile
  case [i | (i : _ : _) <- List.group (List.sort (map Exp.getSignedSubTxId signedSubTxs))] of
    duplicateId : _ -> throwCliError $ TxCmdDuplicateSubTransaction duplicateId
    [] -> pure signedSubTxs

runSubTransactionBuildRawCmd
  :: Cmd.SubTransactionBuildRawCmdArgs
  -> CIO e ()
runSubTransactionBuildRawCmd
  Cmd.SubTransactionBuildRawCmdArgs
    { txIns
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
    , isCborOutCanonical
    , outFile
    } = do
    txInsAndMaybeScriptWits <-
      readSpendScriptWitnesses txIns

    certFilesAndMaybeScriptWits :: [(CertificateFile, Exp.AnyWitness (Exp.LedgerEra Exp.DijkstraEra))] <-
      readCertificateScriptWitnesses certificates

    withdrawalsAndMaybeScriptWits <-
      mapM readWithdrawalScriptWitness withdrawals
    txMetadata <-
      readTxMetadata (convert eon) metadataSchema metadataFiles

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
      conwayEraOnwardsConstraints (convert eon) $
        readVotingProceduresFiles voteFiles

    proposals <-
      readTxGovernanceActions @Exp.DijkstraEra proposalFiles

    certsAndMaybeScriptWits <-
      sequence
        [ (,mSwit) <$> fromEitherIOCli (readFileTextEnvelope (File certFile))
        | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
        ]

    guardCredentials <-
      mapM (readVerificationKeyOrHashOrFileOrScriptHash (\(PaymentKeyHash kh) -> coerceKeyRole kh)) guards

    subTx :: Exp.SubTxBodyContent (Exp.LedgerEra Exp.DijkstraEra) <-
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
      fromEitherCli $ first TxCmdMakeUnsignedTxError $ Exp.makeUnsignedSubTx subTx

    outputSubTx <-
      if isCborOutCanonical == TxCborCanonical
        then fromEitherCli $ do
          cbor <- first TxCmdSubTxCborError $ canonicaliseCborBs (serialiseToCBOR unsignedSubTx)
          first TxCmdSubTxCborError $ deserialiseFromCBOR Exp.AsUnsignedSubTx cbor
        else pure unsignedSubTx
    fromEitherIOCli $ writeFileTextEnvelope outFile Nothing outputSubTx
   where
    eon :: Exp.Era Exp.DijkstraEra
    eon = Exp.DijkstraEra

-- | The sub-transaction analogue of 'constructTxBodyContent'. It shares the
-- value preparation but builds a 'Exp.SubTxBodyContent', which has no fee,
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
  -> Either TxCmdError (Exp.SubTxBodyContent (Exp.LedgerEra era))
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
      -- Only executed Plutus witnesses are restricted. Auxiliary scripts and
      -- reference scripts stored in outputs are not executed by this body.
      let witnesses =
            map snd inputsAndMaybeScriptWits
              ++ map snd certsAndMaybeScriptWits
              ++ map (\(_, _, witness) -> witness) withdrawals
              ++ map snd votingProcedures
              ++ map snd proposals
          languages =
            mapMaybe Exp.getAnyWitnessPlutusLanguage witnesses
              ++ [ Exp.getAnyPlutusScriptWitnessLanguage witness
                 | (_, Exp.AnyScriptWitnessPlutus witness) <- snd valuesWithScriptWits
                 ]
      forM_ languages $ \language ->
        unless (language == L.PlutusV4) $
          Left $
            TxCmdSubTxUnsupportedPlutusLanguage language

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
        ( Exp.defaultSubTxBodyContent
            & Exp.setTxIns inputsAndMaybeScriptWits
            & Exp.setTxInsReference refInputs
            & Exp.setTxOuts txouts
            & maybe id Exp.setTxValidityLowerBound mLowerBound
            & maybe id Exp.setTxValidityUpperBound mUpperBound
            & Exp.setTxMetadata expTxMetadata
            & Exp.setTxAuxScripts auxScripts
            & Exp.setTxWithdrawals (convertWithdrawals withdrawals)
            & maybe id (Exp.setTxProtocolParams . Exp.obtainCommonConstraints (Exp.useEra @era)) mPparams
            & Exp.setTxCertificates
              (Exp.mkTxCertificates Exp.useEra certsAndMaybeScriptWits)
            & Exp.setTxMintValue validatedMintValue
            & Exp.setTxVotingProcedures validatedVotingProcedures
            & Exp.setTxProposalProcedures (Exp.mkTxProposalProcedures txProposals)
            & maybe id Exp.setTxCurrentTreasuryValue (unTxCurrentTreasuryValue <$> mCurrentTreasury)
            & maybe id Exp.setTxTreasuryDonation (unTxTreasuryDonation <$> mTreasuryDonation)
            & Exp.setTxSupplementalDatums suppDatums
            & Exp.setTxGuards guards
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
    unsigned <-
      fromEitherIOCli $ first TxCmdTextEnvError <$> readFileUnsignedSubTx subTxFile

    let keyWits = map (Exp.makeSubTxKeyWitness unsigned) sksShelley
        signed = Exp.signSubTx [] keyWits unsigned

    fromEitherIOCli $ writeFileTextEnvelope outFile Nothing signed

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
          unsigned <-
            fromEitherIOCli $ first TxCmdTextEnvError <$> readFileUnsignedSubTx file
          pure $ Exp.getUnsignedSubTxId unsigned
        InputSignedSubTxFile (File path) -> do
          file <- liftIO $ fileOrPipe path
          signed <-
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

-- | Canonicalising an outer transaction must preserve the body bytes signed by
-- each embedded sub-transaction. Witness encoding is not part of this check.
validateCanonicalSubTransactions
  :: TxCborFormat
  -> ShelleyBasedEra era
  -> Tx era
  -> Either TxCmdError ()
validateCanonicalSubTransactions TxCborNotCanonical _ _ = Right ()
validateCanonicalSubTransactions TxCborCanonical sbe (ShelleyTx _ tx) =
  case sbe of
    ShelleyBasedEraDijkstra ->
      forM_ (Foldable.toList (tx ^. L.bodyTxL . L.subTransactionsTxBodyL)) $ \subTx -> do
        let bodyBytes = originalBytes (subTx ^. L.bodyTxL)
        canonicalBytes <- first TxCmdSubTxCborError $ canonicaliseCborBs bodyBytes
        unless (bodyBytes == canonicalBytes) $
          Left $
            TxCmdNonCanonicalSubTransaction (Exp.getSignedSubTxId (Exp.SignedSubTx subTx))
    _ -> Right ()
