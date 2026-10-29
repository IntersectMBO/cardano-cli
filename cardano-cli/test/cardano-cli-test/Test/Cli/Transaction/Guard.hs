{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cli.Transaction.Guard where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.EraBased.Script.Guard.Option (pTxGuardArguments)
import Cardano.CLI.Type.Common (BalanceTxExecUnits (..))
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Dijkstra.TxBody qualified as L
import Cardano.Ledger.Keys qualified as L
import Cardano.Ledger.Tools qualified as L
import Cardano.Slotting.EpochInfo qualified as Slotting
import Cardano.Slotting.Slot qualified as Slotting
import Cardano.Slotting.Time qualified as Slotting

import Control.Monad (forM, forM_, replicateM, void)
import Data.Aeson qualified as Aeson
import Data.Foldable (toList)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time.Clock.POSIX qualified as Time
import Lens.Micro ((&), (.~), (^.))
import Options.Applicative qualified as Opt
import System.Directory (removeFile)
import System.Exit (ExitCode (..))

import Test.Cardano.CLI.Util

import Hedgehog (Property, (===))
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Test.Cli.Transaction.SubTransaction
  ( nativeScriptFile
  , plutusScriptFile
  , readDijkstraTransaction
  , transactionArgs
  , txIn
  , txOut
  , writeDijkstraProtocolParams
  )

-- Required-signer flags remain compatible in Conway and become key guards in
-- Dijkstra. Repeating a key by file and hash must not create duplicate guards.
hprop_top_level_key_guards :: Property
hprop_top_level_key_guards = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  keyFile <- H.noteTempFile tempDir "payment.skey"
  bodyFile <- H.noteTempFile tempDir "tx.body"
  signedFile <- H.noteTempFile tempDir "tx.signed"
  key <- H.evalIO $ generateSigningKey AsPaymentKey
  H.evalEither =<< H.evalIO (writeFileTextEnvelope (File keyFile) Nothing key)
  let keyHash@(PaymentKeyHash ledgerKeyHash) = verificationKeyHash $ getVerificationKey key
      fileArgs = ["--required-signer", keyFile]
      hashArgs = ["--required-signer-hash", Text.unpack $ serialiseToRawBytesHexText keyHash]
  forM_ [fileArgs, hashArgs, fileArgs ++ hashArgs] $ \args -> do
    void $ execCardanoCLI $ transactionArgs "conway" ++ args ++ ["--out-file", bodyFile]
    ShelleyTx _ conway <-
      H.evalEither
        =<< H.evalIO
          (readFileTextEnvelope (File bodyFile) :: IO (Either (FileError TextEnvelopeError) (Tx ConwayEra)))
    -- Conway still stores the key in required signers, even when supplied twice.
    conway ^. L.bodyTxL . L.reqSignerHashesTxBodyL === Set.singleton (L.coerceKeyRole ledgerKeyHash)
    H.evalIO $ removeFile bodyFile
    void $ execCardanoCLI $ transactionArgs "dijkstra" ++ args ++ ["--out-file", bodyFile]
    ShelleyTx _ dijkstra <- readDijkstraTransaction bodyFile
    -- Both CLI forms produce one key guard in the unified Dijkstra collection.
    toList (dijkstra ^. L.bodyTxL . L.guardsTxBodyL) === [L.KeyHashObj (L.coerceKeyRole ledgerKeyHash)]
    -- Building records the requirement; it must not add a signature yet.
    dijkstra ^. L.witsTxL . L.addrTxWitsL === mempty
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "sign"
        , "--tx-body-file"
        , bodyFile
        , "--signing-key-file"
        , keyFile
        , "--out-file"
        , signedFile
        ]
    ShelleyTx _ signed <- readDijkstraTransaction signedFile
    -- Normal transaction signing supplies the key witness required by the guard.
    Set.size (signed ^. L.witsTxL . L.addrTxWitsL) === 1
    -- Adding that signature must preserve the body, including its guard requirement.
    signed ^. L.bodyTxL === dijkstra ^. L.bodyTxL
    H.evalIO $ removeFile bodyFile
    H.evalIO $ removeFile signedFile

-- Native guards need no protocol parameters or Plutus witness data.
hprop_top_level_native_guard :: Property
hprop_top_level_native_guard = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  outFile <- H.noteTempFile tempDir "tx.body"
  void $
    execCardanoCLI $
      transactionArgs "dijkstra"
        ++ ["--guard-script-file", nativeScriptFile, "--out-file", outFile]
  ShelleyTx _ tx <- readDijkstraTransaction outFile
  -- The native guard credential must match the script included in the witnesses.
  toList (tx ^. L.bodyTxL . L.guardsTxBodyL)
    === map L.ScriptHashObj (Map.keys $ tx ^. L.witsTxL . L.scriptTxWitsL)
  -- The one inline guard supplies exactly one script witness.
  Map.size (tx ^. L.witsTxL . L.scriptTxWitsL) === 1
  -- Native guards do not require Plutus redeemers.
  L.unRedeemers (tx ^. L.witsTxL . L.rdmrsTxWitsL) === mempty

-- Index native guards too, keep spending/guarding redeemers separate, and
-- commit guard data to the script integrity hash before signing.
hprop_top_level_guard_redeemers :: Property
hprop_top_level_guard_redeemers = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  params <- H.noteTempFile tempDir "params.json"
  first <- H.noteTempFile tempDir "first.body"
  second <- H.noteTempFile tempDir "second.body"
  H.evalIO $ writeDijkstraProtocolParams params
  forM_ [(first, "42"), (second, "43")] $ \(outFile, value) ->
    void $
      execCardanoCLI $
        transactionArgs "dijkstra"
          ++ [ "--tx-in-script-file"
             , plutusScriptFile 4
             , "--tx-in-redeemer-value"
             , "0"
             , "--tx-in-execution-units"
             , "(1,2)"
             , "--required-signer-hash"
             , replicate 56 '0'
             , "--guard-script-file"
             , nativeScriptFile
             , "--guard-script-file"
             , plutusScriptFile 4
             , "--guard-redeemer-value"
             , value
             , "--guard-execution-units"
             , "(3,4)"
             , "--protocol-params-file"
             , params
             , "--out-file"
             , outFile
             ]
  ShelleyTx _ tx <- readDijkstraTransaction first
  ShelleyTx _ changed <- readDijkstraTransaction second
  let redeemers = L.unRedeemers $ tx ^. L.witsTxL . L.rdmrsTxWitsL
      datum = toAlonzoData . unsafeHashableScriptData . ScriptDataNumber
  -- The spending redeemer retains its own purpose, index, data and budget.
  Map.lookup (L.DijkstraSpending (L.AsIx 0)) redeemers
    === Just (datum 0, toAlonzoExUnits $ ExecutionUnits 1 2)
  -- The native guard occupies index 0, so the Plutus guard stays at index 1.
  -- Appending the required key guard must not move either script guard.
  Map.lookup (L.DijkstraGuarding (L.AsIx 1)) redeemers
    === Just (datum 42, toAlonzoExUnits $ ExecutionUnits 3 4)
  -- Only the spending input and the Plutus guard contribute redeemers.
  Map.size redeemers === 2
  -- The unified collection contains the native, Plutus and required-key guards.
  length (tx ^. L.bodyTxL . L.guardsTxBodyL) === 3
  -- Spending and guarding reuse one Plutus script, alongside the native script.
  Map.size (tx ^. L.witsTxL . L.scriptTxWitsL) === 2
  H.assert $
    tx ^. L.bodyTxL . L.scriptIntegrityHashTxBodyL
      /= changed ^. L.bodyTxL . L.scriptIntegrityHashTxBodyL
  view <- execCardanoCLI ["debug", "transaction", "view", "--tx-file", first]
  H.assertWith view ("guarding script" `isInfixOf`)

-- Interleaved key and script options must preserve guard order and redeemer indices.
-- Repeating a key among scripts must still produce only one key guard.
hprop_top_level_mixed_guard_ordering :: Property
hprop_top_level_mixed_guard_ordering = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  params <- H.noteTempFile tempDir "params.json"
  outFile <- H.noteTempFile tempDir "tx.body"
  H.evalIO $ writeDijkstraProtocolParams params
  let key = ["--required-signer-hash", replicate 56 '0']
      native = ["--guard-script-file", nativeScriptFile]
      plutus =
        [ "--guard-script-file"
        , plutusScriptFile 4
        , "--guard-redeemer-value"
        , "42"
        , "--guard-execution-units"
        , "(3,4)"
        ]
  transactions <- forM [key ++ native ++ plutus, native ++ key ++ plutus ++ key, native ++ plutus ++ key] $ \guards -> do
    void $
      execCardanoCLI $
        transactionArgs "dijkstra"
          ++ guards
          ++ ["--protocol-params-file", params, "--out-file", outFile]
    ShelleyTx _ tx <- readDijkstraTransaction outFile
    length (tx ^. L.bodyTxL . L.guardsTxBodyL) === 3
    Map.lookup (L.DijkstraGuarding (L.AsIx 1)) (L.unRedeemers $ tx ^. L.witsTxL . L.rdmrsTxWitsL)
      === Just
        ( toAlonzoData $ unsafeHashableScriptData $ ScriptDataNumber 42
        , toAlonzoExUnits $ ExecutionUnits 3 4
        )
    pure tx
  case transactions of
    first : rest -> forM_ rest $ \tx -> do
      tx ^. L.bodyTxL === first ^. L.bodyTxL
      tx ^. L.witsTxL === first ^. L.witsTxL
    [] -> H.failure

-- Reference guards add their UTxO to reference inputs, with no inline script.
hprop_top_level_reference_guards :: Property
hprop_top_level_reference_guards = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  params <- H.noteTempFile tempDir "params.json"
  outFile <- H.noteTempFile tempDir "tx.body"
  H.evalIO $ writeDijkstraProtocolParams params
  let reference = init txIn ++ "1"
  input <-
    H.evalEither $
      deserialiseFromRawBytesHex "63e6a9a8e58e48cc025cae04daaed9d36fc7b70bc292721d9f5057ae37b24981"
  forM_
    [ ("--simple-guard-script-tx-in-reference", [])
    ,
      ( "--guard-tx-in-reference"
      , ["--guard-plutus-script-v4", "--guard-redeemer-value", "42", "--guard-execution-units", "(3,4)"]
      )
    ]
    $ \(referenceFlag, redeemer) -> do
      void $
        execCardanoCLI $
          transactionArgs "dijkstra"
            ++ [referenceFlag, reference, "--guard-script-hash", replicate 56 '0']
            ++ redeemer
            ++ ["--protocol-params-file", params, "--out-file", outFile]
      ShelleyTx _ tx <- readDijkstraTransaction outFile
      -- The guard's reference input must be included in the transaction body.
      tx ^. L.bodyTxL . L.referenceInputsTxBodyL === Set.singleton (toShelleyTxIn $ TxIn input (TxIx 1))
      -- Reference scripts must not also be embedded as inline witnesses.
      Map.size (tx ^. L.witsTxL . L.scriptTxWitsL) === 0
      -- Either reference-script form contributes one guard credential.
      length (tx ^. L.bodyTxL . L.guardsTxBodyL) === 1
      -- A native reference has no redeemer; the Plutus reference has exactly one.
      Map.size (L.unRedeemers $ tx ^. L.witsTxL . L.rdmrsTxWitsL) === if null redeemer then 0 else 1
      H.evalIO $ removeFile outFile

-- Reference flags select the script type and require an explicit Plutus V4
-- version. Omitting a redeemer must never turn Plutus into a native witness.
hprop_top_level_reference_guard_options :: Property
hprop_top_level_reference_guard_options = watchdogProp . propertyOnce $ do
  let reference flag = [flag, txIn, "--guard-script-hash", replicate 56 '0']
      native = reference "--simple-guard-script-tx-in-reference"
      unversioned = reference "--guard-tx-in-reference"
      plutus = unversioned ++ ["--guard-plutus-script-v4"]
      redeemer = ["--guard-redeemer-value", "42"]
      units = ["--guard-execution-units", "(3,4)"]
  forM_ [AutoBalance, ManualBalance] $ \balance -> do
    let witness =
          redeemer ++ case balance of
            AutoBalance -> []
            ManualBalance -> units
        parser = Opt.info (pTxGuardArguments Exp.DijkstraEra balance) mempty
        accepts = isJust . Opt.getParseResult . Opt.execParserPure Opt.defaultPrefs parser
    forM_ [native, plutus ++ witness, native ++ plutus ++ witness, plutus ++ witness ++ native] $ \args -> do
      H.annotateShow args
      accepts args === True
    forM_ [plutus, native ++ redeemer, native ++ units, native ++ redeemer ++ units, plutus ++ units] $ \args -> do
      H.annotateShow args
      accepts args === False
    forM_
      ( [unversioned ++ witness, native ++ ["--guard-plutus-script-v4"]]
          ++ [unversioned ++ ["--guard-plutus-script-v" ++ show version] ++ witness | version <- [1 .. 3 :: Int]]
      )
      $ \args -> do
        H.annotateShow args
        accepts args === False
    case balance of
      AutoBalance -> accepts (plutus ++ redeemer ++ units) === False
      ManualBalance -> accepts (plutus ++ redeemer) === False

hprop_top_level_guard_rejects_invalid_witnesses :: Property
hprop_top_level_guard_rejects_invalid_witnesses = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  outFile <- H.noteTempFile tempDir "unused"
  let budget = ["--guard-redeemer-value", "0", "--guard-execution-units", "(1,2)"]
      cases =
        [ ["--guard-script-file", plutusScriptFile 4]
        , ["--guard-script-file", plutusScriptFile 4, "--guard-redeemer-value", "0"]
        , ["--guard-script-file", nativeScriptFile] ++ budget
        , ["--guard-script-file", nativeScriptFile, "--guard-script-file", nativeScriptFile]
        ]
          ++ [["--guard-script-file", plutusScriptFile v] ++ budget | v <- [1 .. 3]]
  forM_ cases $ \args -> do
    (code, _, _) <- execDetailCardanoCLI $ transactionArgs "dijkstra" ++ args ++ ["--out-file", outFile]
    -- Reject missing Plutus data/budgets, native redeemers, duplicates and older scripts.
    code === ExitFailure 1

hprop_top_level_guard_options :: Property
hprop_top_level_guard_options = watchdogProp . propertyOnce $ do
  forM_ ["build", "build-raw", "build-estimate"] $ \command -> do
    help <- execCardanoCLI ["dijkstra", "transaction", command, "--help"]
    H.assertWith help ("--guard-script-file" `isInfixOf`)
    H.assertWith help ("--guard-tx-in-reference" `isInfixOf`)
    H.assertWith help ("--guard-plutus-script-v4" `isInfixOf`)
    H.assertWith help ("--simple-guard-script-tx-in-reference" `isInfixOf`)
    -- Online build computes budgets; raw and estimated builds require explicit units.
    ("--guard-execution-units" `isInfixOf` help) === (command /= "build")
    forM_ ["conway", "latest"] $ \era -> do
      (code, _, stderr) <-
        execDetailCardanoCLI [era, "transaction", command, "--guard-script-file", nativeScriptFile]
      -- Conway and the current latest-era alias must reject Dijkstra guard flags.
      code === ExitFailure 1
      H.assertWith stderr ("Invalid option `--guard-script-file'" `isInfixOf`)

-- Online build validates witnesses before contacting the node, and accepts
-- a redeemer without an execution budget (which is calculated by the API).
hprop_top_level_guard_build_validation :: Property
hprop_top_level_guard_build_validation = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  outFile <- H.noteTempFile tempDir "unused"
  socket <- H.noteTempFile tempDir "missing.socket"
  forM_ [(nativeScriptFile, "does not accept a redeemer"), (plutusScriptFile 3, "must be Plutus V4")] $ \(script, message) -> do
    (code, _, stderr) <-
      execDetailCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "build"
        , "--testnet-magic"
        , "42"
        , "--socket-path"
        , socket
        , "--tx-in"
        , txIn
        , "--change-address"
        , takeWhile (/= '+') txOut
        , "--guard-script-file"
        , script
        , "--guard-redeemer-value"
        , "0"
        , "--out-file"
        , outFile
        ]
    -- Invalid guard witnesses must fail before attempting the missing node socket.
    code === ExitFailure 1
    H.assertWith stderr (message `isInfixOf`)

-- Offline balancing must preserve the guarding budget and charge for the
-- extra script/redeemer bytes, even without a script spending input.
hprop_top_level_guard_build_estimate :: Property
hprop_top_level_guard_build_estimate = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  paramsFile <- H.noteTempFile tempDir "params.json"
  plainFile <- H.noteTempFile tempDir "plain.body"
  guardFile <- H.noteTempFile tempDir "guard.body"
  largerBudgetFile <- H.noteTempFile tempDir "larger-budget.body"
  memoryPrice <- H.evalMaybe $ L.boundRational 2
  stepPrice <- H.evalMaybe $ L.boundRational 3
  let params =
        (L.PParams L.emptyPParamsIdentity :: L.PParams (Exp.LedgerEra Exp.DijkstraEra))
          & L.ppMaxTxSizeL .~ 16384
          & L.ppTxFeePerByteL .~ L.CoinPerByte (L.CompactCoin 44)
          & L.ppTxFeeFixedL .~ 155381
          & L.ppPricesL .~ L.Prices memoryPrice stepPrice
      guardArgs budget =
        [ "--guard-script-file"
        , plutusScriptFile 4
        , "--guard-redeemer-value"
        , "42"
        , "--guard-execution-units"
        , budget
        ]
  H.evalIO $ Aeson.encodeFile paramsFile params
  forM_
    [ (plainFile, [])
    , (guardFile, guardArgs "(3,4)")
    , (largerBudgetFile, guardArgs "(6,8)")
    ]
    $ \(outFile, guards) ->
      void $
        execCardanoCLI $
          [ "dijkstra"
          , "transaction"
          , "build-estimate"
          , "--tx-in"
          , txIn
          , "--total-utxo-value"
          , "100000000"
          , "--shelley-key-witnesses"
          , "1"
          , "--protocol-params-file"
          , paramsFile
          , "--change-address"
          , takeWhile (/= '+') txOut
          , "--out-file"
          , outFile
          ]
            ++ guards
  ShelleyTx _ plain <- readDijkstraTransaction plainFile
  ShelleyTx _ guarded <- readDijkstraTransaction guardFile
  ShelleyTx _ largerBudget <- readDijkstraTransaction largerBudgetFile
  H.assert $ guarded ^. L.bodyTxL . L.feeTxBodyL > plain ^. L.bodyTxL . L.feeTxBodyL
  -- These budgets have equal CBOR sizes. Only execution costs change:
  -- three extra steps at 3 lovelace and four extra memory units at 2 lovelace.
  (largerBudget ^. L.bodyTxL . L.feeTxBodyL) - (guarded ^. L.bodyTxL . L.feeTxBodyL) === L.Coin 17
  let datum = toAlonzoData $ unsafeHashableScriptData $ ScriptDataNumber 42
  -- Fee estimation must retain the Plutus guard's redeemer and explicit budget.
  Map.lookup (L.DijkstraGuarding (L.AsIx 0)) (L.unRedeemers $ guarded ^. L.witsTxL . L.rdmrsTxWitsL)
    === Just (datum, toAlonzoExUnits $ ExecutionUnits 3 4)
  Map.lookup
    (L.DijkstraGuarding (L.AsIx 0))
    (L.unRedeemers $ largerBudget ^. L.witsTxL . L.rdmrsTxWitsL)
    === Just (datum, toAlonzoExUnits $ ExecutionUnits 6 8)

-- Like ordinary native reference scripts, reference guards use the explicit
-- witness override to account for signatures inside the referenced script.
hprop_top_level_native_reference_guard_witness_override :: Property
hprop_top_level_native_reference_guard_witness_override = watchdogProp . propertyOnce $ do
  keys <- H.evalIO $ replicateM 3 $ generateSigningKey AsPaymentKey
  address <-
    H.evalMaybe $
      deserialiseAddress (AsAddressInEra AsDijkstraEra) $
        Text.pack $
          takeWhile (/= '+') txOut
  inputId <-
    H.evalEither $
      deserialiseFromRawBytesHex "63e6a9a8e58e48cc025cae04daaed9d36fc7b70bc292721d9f5057ae37b24981"
  let input = TxIn inputId (TxIx 0)
      reference = TxIn inputId (TxIx 1)
      native =
        toDijkstraNativeScript $
          RequireAllOf [RequireSignature (verificationKeyHash $ getVerificationKey key) | key <- keys]
      credential = L.ScriptHashObj $ Exp.hashSimpleScript $ Exp.SimpleScript native
      params =
        (L.PParams L.emptyPParamsIdentity :: L.PParams (Exp.LedgerEra Exp.DijkstraEra))
          & L.ppProtocolVersionL .~ L.ProtVer (L.eraProtVerLow @(Exp.LedgerEra Exp.DijkstraEra)) 0
          & L.ppMaxTxSizeL .~ 16384
          & L.ppTxFeePerByteL .~ L.CoinPerByte (L.CompactCoin 44)
          & L.ppTxFeeFixedL .~ 155381
      funding = L.mkBasicTxOut (toShelleyAddr address) (L.MaryValue (L.Coin 100000000) mempty)
      referenceOutput = funding & L.referenceScriptTxOutL .~ L.SJust (L.fromNativeScript native)
      utxo =
        L.UTxO $ Map.fromList [(toShelleyTxIn input, funding), (toShelleyTxIn reference, referenceOutput)]
      systemStart = SystemStart $ Time.posixSecondsToUTCTime 0
      epochInfo = LedgerEpochInfo $ Slotting.fixedEpochInfo (Slotting.EpochSize 100) (Slotting.mkSlotLength 1000)
  guards <-
    H.evalEither $
      Exp.mkTxGuards [(credential, Just $ Exp.NativeGuardWitness $ Exp.SReferenceScript reference)]
  let body =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(input, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxInsReference (Exp.TxInsReference [reference] Set.empty)
          & Exp.setTxGuards guards
          & Exp.setTxProtocolParams params
  (Exp.UnsignedTx tx, balanced) <-
    H.evalEither $
      Exp.makeTransactionBodyAutoBalance
        systemStart
        epochInfo
        params
        mempty
        mempty
        utxo
        body
        address
        (Just 3)
  H.assert $ tx ^. L.bodyTxL . L.feeTxBodyL >= L.calcMinFeeTx utxo params tx 3
  tx ^. L.witsTxL . L.scriptTxWitsL === mempty
  tx ^. L.bodyTxL . L.referenceInputsTxBodyL === Set.singleton (toShelleyTxIn reference)
  Exp.txGuards balanced === guards
