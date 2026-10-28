{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Runners for Dijkstra sub-transactions: the @sub-transaction@
-- command group, and reading signed sub-transactions for embedding in a
-- top-level transaction.
module Cardano.CLI.EraBased.Transaction.SubTransaction.Run
  ( runSubTransactionCmds
  , runSubTransactionBuildRawCmd
  , runSubTransactionSignCmd
  , runSubTransactionWitnessCmd
  , runSubTransactionAssembleCmd
  , runSubTransactionTxIdCmd
  , readSignedSubTransactions
  , validateCanonicalSubTransactions
  )
where

import Cardano.Api
import Cardano.Api.Experimental (obtainCommonConstraints)
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.AnyScriptWitness qualified as Exp
import Cardano.Api.Experimental.Plutus qualified as Exp.Plutus
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Genesis.Internal.Common (readProtocolParameters)
import Cardano.CLI.EraBased.Script.Certificate.Read
import Cardano.CLI.EraBased.Script.Mint.Read
import Cardano.CLI.EraBased.Script.Proposal.Read
import Cardano.CLI.EraBased.Script.Read.Common (readScriptDataOrFile)
import Cardano.CLI.EraBased.Script.Spend.Read
import Cardano.CLI.EraBased.Script.Vote.Read
import Cardano.CLI.EraBased.Script.Withdrawal.Read
import Cardano.CLI.EraBased.Transaction.Command (accountBalanceIntervalOptionName)
import Cardano.CLI.EraBased.Transaction.Internal.Common
import Cardano.CLI.EraBased.Transaction.SubTransaction.Command qualified as Cmd
import Cardano.CLI.Json.Encode qualified as Json
import Cardano.CLI.Read
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.TxCmdError
import Cardano.CLI.Type.Error.TxValidationError
import Cardano.CLI.Type.Key (readVerificationKeyOrHashOrTextEnvFile)
import Cardano.Ledger.Core qualified as L (fromNativeScript)
import Cardano.Ledger.Dijkstra.Scripts qualified as L
import Cardano.Ledger.Dijkstra.TxBody qualified as L
import Cardano.Ledger.Hashes (DataHash, hashAnnotated, originalBytes)
import Cardano.Ledger.Keys (coerceKeyRole)
import Cardano.Ledger.Plutus.Language qualified as L
import Cardano.Ledger.State (verifyWitVKey)

import RIO hiding (toList)

import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.OSet.Strict (OSet)
import Data.OSet.Strict qualified as OSet
import Data.Set qualified as Set
import Vary qualified

runSubTransactionCmds :: Cmd.SubTransactionCmds -> CIO e ()
runSubTransactionCmds = \case
  Cmd.SubTransactionBuildRawCmd args -> runSubTransactionBuildRawCmd args
  Cmd.SubTransactionSignCmd args -> runSubTransactionSignCmd args
  Cmd.SubTransactionWitnessCmd args -> runSubTransactionWitnessCmd args
  Cmd.SubTransactionAssembleCmd args -> runSubTransactionAssembleCmd args
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
  case findDuplicates (map Exp.getSignedSubTxId signedSubTxs) of
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
    , directDeposits
    , accountBalanceIntervals
    , metadataSchema
    , scriptFiles
    , metadataFiles
    , mProtocolParamsFile
    , voteFiles
    , proposalFiles
    , mCurrentTreasuryValue
    , mTreasuryDonation
    , guards
    , topLevelGuards
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

    guardWitnesses <- mapM readGuard guards
    let guardCredentials = [credential | (credential, _, _) <- guardWitnesses]
        duplicates = findDuplicates guardCredentials
    unless (null duplicates) $ throwCliError $ TxCmdDuplicateGuards duplicates

    topLevelGuardEntries <- mapM readTopLevelGuard topLevelGuards
    let duplicateTopLevelGuards = findDuplicates (map fst topLevelGuardEntries)
    unless (null duplicateTopLevelGuards) $
      throwCliError $
        TxCmdDuplicateTopLevelGuards duplicateTopLevelGuards
    let requiredTopLevelGuards = Map.fromList topLevelGuardEntries

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
          directDeposits
          accountBalanceIntervals
          txAuxScripts
          txMetadata
          votingProceduresAndMaybeScriptWits
          proposals
          mCurrentTreasuryValue
          mTreasuryDonation
          supplementalDatums
          (OSet.fromList guardCredentials)
          requiredTopLevelGuards

    unsigned <-
      fromEitherCli $ first TxCmdMakeUnsignedTxError $ Exp.makeUnsignedSubTx subTx
    unsignedSubTx <- fromEitherCli $ addGuardWitnesses pparams guardWitnesses unsigned

    outputSubTx <-
      if isCborOutCanonical == TxCborCanonical
        then fromEitherCli $ do
          validateCanonicalSubTx unsignedSubTx
          cbor <- first TxCmdSubTxCborError $ canonicaliseCborBs (serialiseToCBOR unsignedSubTx)
          first TxCmdSubTxCborError $ deserialiseFromCBOR Exp.AsUnsignedSubTx cbor
        else pure unsignedSubTx
    fromEitherIOCli $ writeFileTextEnvelope outFile Nothing outputSubTx
   where
    eon :: Exp.Era Exp.DijkstraEra
    eon = Exp.DijkstraEra

type GuardWitness =
  ( L.Credential L.Guard
  , Maybe (L.Script (Exp.LedgerEra Exp.DijkstraEra))
  , Maybe (L.Data (Exp.LedgerEra Exp.DijkstraEra), L.ExUnits)
  )

-- | A file identifies the script language; a bare hash may refer to a script
-- supplied elsewhere, whose language cannot always be checked offline.
readGuard :: Cmd.Guard -> CIO e GuardWitness
readGuard = \case
  Cmd.KeyGuard key -> do
    PaymentKeyHash keyHash <- readVerificationKeyOrHashOrTextEnvFile key
    pure (L.KeyHashObj (coerceKeyRole keyHash), Nothing, Nothing)
  Cmd.ScriptGuard source mRedeemer -> do
    (credential, script) <- case source of
      Cmd.GuardScriptHash (ScriptHash scriptHash) -> pure (L.ScriptHashObj scriptHash, Nothing)
      Cmd.GuardScriptFile (File path) -> do
        ledgerScript <- readGuardScriptFile TxCmdSubTxUnsupportedPlutusLanguage path
        pure (L.ScriptHashObj (L.hashScript ledgerScript), Just ledgerScript)
    forM_ script $ \s -> fromEitherCli $ validateGuardRedeemer credential s (isJust mRedeemer)
    redeemer <- forM mRedeemer $ \(redeemerData, units) -> do
      value <- fromExceptTCli $ readScriptDataOrFile redeemerData
      pure (toAlonzoData value, toAlonzoExUnits units)
    pure (credential, script, redeemer)

-- | Both guard forms accept native scripts or Plutus V4 scripts.
readGuardScriptFile
  :: (L.Language -> TxCmdError)
  -> FilePath
  -> CIO e (L.Script (Exp.LedgerEra Exp.DijkstraEra))
readGuardScriptFile languageError path = do
  script <- readAnyScript @_ @Exp.DijkstraEra path
  case script of
    Exp.AnySimpleScript (Exp.SimpleScript native) -> pure $ L.fromNativeScript native
    Exp.AnyPlutusScript plutus -> do
      let language = Exp.Plutus.plutusScriptInEraLanguage plutus
      unless (language == L.PlutusV4) $
        throwCliError $
          languageError language
      pure $ Exp.Plutus.plutusScriptInEraToScript plutus

validateGuardRedeemer
  :: L.Credential L.Guard
  -> L.Script (Exp.LedgerEra Exp.DijkstraEra)
  -> Bool
  -> Either TxCmdError ()
validateGuardRedeemer credential script hasRedeemer
  | isJust (L.getNativeScript script) =
      when hasRedeemer $ Left $ TxCmdNativeGuardWithRedeemer credential
  | otherwise =
      unless hasRedeemer $ Left $ TxCmdPlutusGuardMissingRedeemer credential

-- | Guard credentials are passed to the API without witnesses below.
-- Add their witnesses before canonicalisation/signing and update the script
-- integrity hash to cover the complete redeemer set. Guard indices include key
-- and native-script guards, and preserve the body's OSet order.
addGuardWitnesses
  :: Maybe (L.PParams (Exp.LedgerEra Exp.DijkstraEra))
  -> [GuardWitness]
  -> Exp.UnsignedSubTx
  -> Either TxCmdError Exp.UnsignedSubTx
addGuardWitnesses pparams guards (Exp.UnsignedSubTx tx) = do
  let scripts =
        Map.fromList [(L.hashScript script, script) | (_, Just script, _) <- guards]
          <> tx ^. L.witsTxL . L.scriptTxWitsL
      guardRedeemers =
        Map.fromList
          [ (L.GuardingPurpose (L.AsIx index), redeemer)
          | (index, (_, _, Just redeemer)) <- zip [0 ..] guards
          ]
  forM_ guards $ \(credential, _, redeemer) -> case credential of
    L.KeyHashObj _ -> pure ()
    L.ScriptHashObj scriptHash ->
      forM_ (Map.lookup scriptHash scripts) $ \script ->
        validateGuardRedeemer credential script (isJust redeemer)
  let redeemers = tx ^. L.witsTxL . L.rdmrsTxWitsL <> L.Redeemers guardRedeemers
      witnesses =
        tx
          ^. L.witsTxL
            & L.scriptTxWitsL
          .~ scripts
            & L.rdmrsTxWitsL
          .~ redeemers
      witnessed = tx & L.witsTxL .~ witnesses
  if Map.null guardRedeemers
    then pure $ Exp.UnsignedSubTx witnessed
    else do
      pp <- maybe (Left $ TxCmdMakeUnsignedTxError Exp.MakeUnsignedTxMissingProtocolParams) Right pparams
      -- All executed Plutus scripts in a sub-transaction must be V4.
      let integrity =
            getScriptIntegrityHash
              (BuildTxWith $ Just $ LedgerProtocolParameters pp)
              (Set.singleton L.PlutusV4)
              ( TxBodyScriptData
                  AlonzoEraOnwardsDijkstra
                  (witnessed ^. L.witsTxL . L.datsTxWitsL)
                  (witnessed ^. L.witsTxL . L.rdmrsTxWitsL)
              )
      pure $ Exp.UnsignedSubTx $ witnessed & L.bodyTxL . L.scriptIntegrityHashTxBodyL .~ integrity

-- | Record a requirement on the enclosing transaction without adding a witness.
-- File inputs permit local datum validation; hashes leave it to the ledger.
readTopLevelGuard
  :: Cmd.TopLevelGuard
  -> CIO e (L.Credential L.Guard, L.StrictMaybe (L.Data (Exp.LedgerEra Exp.DijkstraEra)))
readTopLevelGuard = \case
  Cmd.TopLevelKeyGuard key -> do
    PaymentKeyHash keyHash <- readVerificationKeyOrHashOrTextEnvFile key
    pure (L.KeyHashObj (coerceKeyRole keyHash), L.SNothing)
  Cmd.TopLevelScriptGuard source mDatum -> do
    scriptHash <- case source of
      Cmd.GuardScriptHash (ScriptHash hash) -> pure hash
      Cmd.GuardScriptFile (File path) -> do
        script <- readGuardScriptFile (TxCmdTopLevelGuardUnsupportedPlutusLanguage path) path
        if isJust (L.getNativeScript script)
          then when (isJust mDatum) $ throwCliError $ TxCmdTopLevelGuardUnexpectedDatum path
          else when (isNothing mDatum) $ throwCliError $ TxCmdTopLevelGuardMissingDatum path
        pure $ L.hashScript script
    datum <- forM mDatum $ \d -> toAlonzoData <$> fromExceptTCli (readScriptDataOrFile d)
    pure (L.ScriptHashObj scriptHash, maybe L.SNothing L.SJust datum)

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
  -> [(StakeAddress, Lovelace)]
  -- ^ Direct ADA deposits as recipient stake addresses and amounts in lovelace.
  -> [(StakeAddress, L.AccountBalanceInterval (Exp.LedgerEra era))]
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
  -> Map.Map (L.Credential L.Guard) (L.StrictMaybe (L.Data (Exp.LedgerEra era)))
  -- ^ Required top-level guards
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
  directDeposits
  accountBalanceIntervals
  txAuxScripts
  txMetadata
  votingProcedures
  proposals
  mCurrentTreasury
  mTreasuryDonation
  suppDatums
  guards
  requiredTopLevelGuards =
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

      validatedDirectDeposits <- convertDirectDeposits directDeposits
      validatedIntervals <-
        convertAccountBalanceIntervals accountBalanceIntervalOptionName accountBalanceIntervals
      validatedGuards <-
        first TxCmdGuardsError $
          obtainCommonConstraints (Exp.useEra @era) $
            Exp.mkTxGuards [(credential, Nothing) | credential <- Foldable.toList guards]
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
            & Exp.setTxDirectDeposits validatedDirectDeposits
            & Exp.setTxAccountBalanceIntervals validatedIntervals
            & maybe id (Exp.setTxProtocolParams . Exp.obtainCommonConstraints (Exp.useEra @era)) mPparams
            & Exp.setTxCertificates
              (Exp.mkTxCertificates Exp.useEra certsAndMaybeScriptWits)
            & Exp.setTxMintValue validatedMintValue
            & Exp.setTxVotingProcedures validatedVotingProcedures
            & Exp.setTxProposalProcedures (Exp.mkTxProposalProcedures txProposals)
            & maybe id (Exp.setTxCurrentTreasuryValue . unTxCurrentTreasuryValue) mCurrentTreasury
            & maybe id (Exp.setTxTreasuryDonation . unTxTreasuryDonation) mTreasuryDonation
            & Exp.setTxSupplementalDatums suppDatums
            & Exp.setTxGuards validatedGuards
            & Exp.setTxRequiredTopLevelGuards requiredTopLevelGuards
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

runSubTransactionWitnessCmd
  :: Cmd.SubTransactionWitnessCmdArgs
  -> CIO e ()
runSubTransactionWitnessCmd
  Cmd.SubTransactionWitnessCmdArgs
    { subTxFile = File subTxFilePath
    , witnessSigningData
    , outFile
    } = do
    subTxFile <- liftIO $ fileOrPipe subTxFilePath
    unsigned <-
      fromEitherIOCli $ first TxCmdTextEnvError <$> readFileUnsignedSubTx subTxFile
    signingWitness <-
      fromEitherIOCli $
        first TxCmdReadWitnessSigningDataError <$> readWitnessSigningData witnessSigningData
    key <- case categoriseSomeSigningWitness signingWitness of
      AByronWitness _ -> throwCliError TxCmdSubTxByronWitnessUnsupported
      AShelleyKeyWitness key -> pure key
    let witness = ShelleyKeyWitness ShelleyBasedEraDijkstra $ Exp.makeSubTxKeyWitness unsigned key
    fromEitherIOCli $
      first TxCmdWriteFileError <$> writeTxWitnessFileTextEnvelope ShelleyBasedEraDijkstra outFile witness

runSubTransactionAssembleCmd
  :: Cmd.SubTransactionAssembleCmdArgs
  -> CIO e ()
runSubTransactionAssembleCmd
  Cmd.SubTransactionAssembleCmdArgs
    { subTxFile = File subTxFilePath
    , witnessFiles
    , outFile
    } = do
    subTxFile <- liftIO $ fileOrPipe subTxFilePath
    unsigned <-
      fromEitherIOCli $ first TxCmdTextEnvError <$> readFileUnsignedSubTx subTxFile
    let Exp.UnsignedSubTx unsignedTx = unsigned
        bodyHash = L.extractHash $ hashAnnotated (unsignedTx ^. L.bodyTxL)
    keyWits <- forM witnessFiles $ \(WitnessFile path) -> do
      witnessFile <- liftIO $ fileOrPipe path
      witness <-
        fromEitherIOCli $
          first TxCmdTextEnvError
            <$> readFileOrPipeTextEnvelopeAnyOf [FromSomeType (AsKeyWitness AsDijkstraEra) id] witnessFile
      case witness of
        ShelleyKeyWitness _ keyWit -> do
          unless (verifyWitVKey bodyHash keyWit) $
            throwCliError $
              TxCmdSubTxInvalidWitness (WitnessFile path)
          pure keyWit
        ShelleyBootstrapWitness{} -> throwCliError TxCmdSubTxByronWitnessUnsupported
    -- Preserve the body bytes covered by the signatures, as in 'sign'.
    fromEitherIOCli $ writeFileTextEnvelope outFile Nothing $ Exp.signSubTx [] keyWits unsigned

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

-- | Canonicalisation must preserve the bytes behind hashes already stored in
-- the sub-transaction body. Datums may also be referenced by existing UTxOs.
validateCanonicalSubTx :: Exp.UnsignedSubTx -> Either TxCmdError ()
validateCanonicalSubTx unsigned@(Exp.UnsignedSubTx tx) = do
  forM_ (tx ^. L.auxDataTxL) $ \aux ->
    check "auxiliary data (metadata and auxiliary scripts)" (originalBytes aux)
  check "redeemers" $ originalBytes (tx ^. L.witsTxL . L.rdmrsTxWitsL)
  check "datums" $ originalBytes (tx ^. L.witsTxL . L.datsTxWitsL)
  forM_ (tx ^. L.witsTxL . L.scriptTxWitsL) $ \script ->
    -- Plutus scripts are opaque byte strings; native script structures can
    -- be rewritten, changing the script hashes used by credentials/policies.
    when (isJust $ L.getNativeScript script) $
      check "native script" (originalBytes script)
 where
  check component bytes = do
    canonical <- first TxCmdSubTxCborError $ canonicaliseCborBs bytes
    unless (bytes == canonical) $
      Left $
        TxCmdNonCanonicalSubTxComponent (Exp.getUnsignedSubTxId unsigned) component

-- | Canonicalising an outer transaction must preserve both the signed body and
-- the hashed payloads of every embedded sub-transaction.
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
        validateCanonicalSubTx $ Exp.UnsignedSubTx subTx
    _ -> Right ()
