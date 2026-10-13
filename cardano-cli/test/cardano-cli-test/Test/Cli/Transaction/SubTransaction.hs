{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cli.Transaction.SubTransaction where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.Dijkstra.TxBody qualified as L

import Control.Monad (forM_, void)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Lens.Micro ((^.))
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H

signingKey :: FilePath
signingKey = "test/cardano-cli-test/files/input/sub-transaction/payment.skey"

nativeScriptFile :: FilePath
nativeScriptFile = "test/cardano-cli-test/files/input/shelley/multisig/scripts/all"

txIn :: String
txIn = "63e6a9a8e58e48cc025cae04daaed9d36fc7b70bc292721d9f5057ae37b24981#0"

txOut :: String
txOut = "addr_test1vp0t4dfa9ktc2uvv7sg9leafuhtwyu0xcj4q4kf5pqkpjwqhklklg+5000000"

-- | Sub-transaction signing rejects the Byron-only address option at parse time.
hprop_dijkstra_sub_transaction_sign_rejects_address :: Property
hprop_dijkstra_sub_transaction_sign_rejects_address =
  watchdogProp . propertyOnce $ do
    (exitCode, _, stderr) <-
      execDetailCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "sub-transaction"
        , "sign"
        , "--sub-tx-file"
        , "unused"
        , "--signing-key-file"
        , signingKey
        , "--address"
        , "unused"
        , "--out-file"
        , "unused"
        ]
    exitCode H.=== ExitFailure 1
    H.assertWith stderr ("Invalid option `--address'" `isInfixOf`)

-- | Sub-transactions exist from Dijkstra onwards, so the command group must not
-- parse in Conway.
-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/conway has no sub transaction commands/"'@
hprop_conway_has_no_sub_transaction_commands :: Property
hprop_conway_has_no_sub_transaction_commands =
  watchdogProp . propertyOnce $ do
    (exitCode, _stdout, stderr) <-
      H.noteShowM $
        execDetailCardanoCLI
          [ "conway"
          , "transaction"
          , "sub-transaction"
          , "build-raw"
          , "--tx-in"
          , txIn
          , "--tx-out"
          , txOut
          , "--out-file"
          , "unused"
          ]
    exitCode H.=== ExitFailure 1
    H.assertWith stderr ("Invalid argument `sub-transaction'" `isInfixOf`)

-- | The @--signed-sub-tx-file@ option on @build-raw@ must not parse in Conway.
-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/conway build raw has no sub transaction option/"'@
hprop_conway_build_raw_has_no_sub_transaction_option :: Property
hprop_conway_build_raw_has_no_sub_transaction_option =
  watchdogProp . propertyOnce $ do
    (exitCode, _stdout, stderr) <-
      H.noteShowM $
        execDetailCardanoCLI
          [ "conway"
          , "transaction"
          , "build-raw"
          , "--tx-in"
          , txIn
          , "--tx-out"
          , txOut
          , "--fee"
          , "1"
          , "--signed-sub-tx-file"
          , "unused"
          , "--out-file"
          , "unused"
          ]
    exitCode H.=== ExitFailure 1
    H.assertWith stderr ("Invalid option `--signed-sub-tx-file'" `isInfixOf`)

-- | The same sub-transaction given twice is an error rather than silently
-- collapsing into one.
-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/dijkstra build raw rejects duplicate sub transaction/"'@
hprop_dijkstra_build_raw_rejects_duplicate_sub_transaction :: Property
hprop_dijkstra_build_raw_rejects_duplicate_sub_transaction =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    unsignedFile <- H.noteTempFile tempDir "sub-tx.unsigned"
    signedFile <- H.noteTempFile tempDir "sub-tx.signed"
    outFile <- H.noteTempFile tempDir "tx.body"
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "sub-transaction"
        , "build-raw"
        , "--tx-in"
        , txIn
        , "--tx-out"
        , txOut
        , "--out-file"
        , unsignedFile
        ]
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "sub-transaction"
        , "sign"
        , "--sub-tx-file"
        , unsignedFile
        , "--signing-key-file"
        , signingKey
        , "--out-file"
        , signedFile
        ]
    (exitCode, _stdout, stderr) <-
      H.noteShowM $
        execDetailCardanoCLI
          [ "dijkstra"
          , "transaction"
          , "build-raw"
          , "--tx-in"
          , txIn
          , "--tx-out"
          , txOut
          , "--fee"
          , "200000"
          , "--signed-sub-tx-file"
          , signedFile
          , "--signed-sub-tx-file"
          , signedFile
          , "--out-file"
          , outFile
          ]
    exitCode H.=== ExitFailure 1
    H.assertWith stderr ("was provided more than once" `isInfixOf`)

-- | Only signed sub-transactions can be embedded.
-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/dijkstra build raw rejects unsigned sub transaction/"'@
hprop_dijkstra_build_raw_rejects_unsigned_sub_transaction :: Property
hprop_dijkstra_build_raw_rejects_unsigned_sub_transaction =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    unsignedFile <- H.noteTempFile tempDir "sub-tx.unsigned"
    outFile <- H.noteTempFile tempDir "tx.body"
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "sub-transaction"
        , "build-raw"
        , "--tx-in"
        , txIn
        , "--tx-out"
        , txOut
        , "--out-file"
        , unsignedFile
        ]
    (exitCode, _stdout, stderr) <-
      H.noteShowM $
        execDetailCardanoCLI
          [ "dijkstra"
          , "transaction"
          , "build-raw"
          , "--tx-in"
          , txIn
          , "--tx-out"
          , txOut
          , "--fee"
          , "200000"
          , "--signed-sub-tx-file"
          , unsignedFile
          , "--out-file"
          , outFile
          ]
    exitCode H.=== ExitFailure 1
    H.assertWith stderr ("Unwitnessed SubTx DijkstraEra" `isInfixOf`)

-- | Byron (bootstrap) witnesses need a top-level body and are refused.
-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/dijkstra sub transaction sign rejects byron key/"'@
hprop_dijkstra_sub_transaction_sign_rejects_byron_key :: Property
hprop_dijkstra_sub_transaction_sign_rejects_byron_key =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    unsignedFile <- H.noteTempFile tempDir "sub-tx.unsigned"
    rawByronKeyFile <- H.noteTempFile tempDir "byron.raw.skey"
    byronKeyFile <- H.noteTempFile tempDir "byron.skey"
    outFile <- H.noteTempFile tempDir "sub-tx.signed"
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "sub-transaction"
        , "build-raw"
        , "--tx-in"
        , txIn
        , "--tx-out"
        , txOut
        , "--out-file"
        , unsignedFile
        ]
    -- A Byron signing key in the TextEnvelope format that `sign` reads
    void $
      execCardanoCLI
        [ "byron"
        , "key"
        , "keygen"
        , "--secret"
        , rawByronKeyFile
        ]
    void $
      execCardanoCLI
        [ "key"
        , "convert-byron-key"
        , "--byron-payment-key-type"
        , "--byron-signing-key-file"
        , rawByronKeyFile
        , "--out-file"
        , byronKeyFile
        ]
    (exitCode, _stdout, stderr) <-
      H.noteShowM $
        execDetailCardanoCLI
          [ "dijkstra"
          , "transaction"
          , "sub-transaction"
          , "sign"
          , "--sub-tx-file"
          , unsignedFile
          , "--signing-key-file"
          , byronKeyFile
          , "--out-file"
          , outFile
          ]
    exitCode H.=== ExitFailure 1
    H.assertWith
      stderr
      ("Byron (bootstrap) witnesses are not supported for sub-transactions" `isInfixOf`)

-- | A large output list uses indefinite-length CBOR by default. Canonicalising
-- the batch must fail rather than invalidate the embedded signature.
hprop_dijkstra_canonical_output_rejects_noncanonical_sub_transaction :: Property
hprop_dijkstra_canonical_output_rejects_noncanonical_sub_transaction =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    unsignedFile <- H.noteTempFile tempDir "sub.unsigned"
    signedFile <- H.noteTempFile tempDir "sub.signed"
    bodyFile <- H.noteTempFile tempDir "tx.body"
    txFile <- H.noteTempFile tempDir "tx.signed"
    witnessFile <- H.noteTempFile tempDir "tx.witness"
    outFile <- H.noteTempFile tempDir "canonical.out"
    -- Build without --out-canonical-cbor. The 24 outputs force the default
    -- encoder to use an indefinite-length list, so canonicalisation changes
    -- this sub-transaction's body bytes rather than leaving them unchanged.
    void $
      execCardanoCLI $
        ["dijkstra", "transaction", "sub-transaction", "build-raw", "--tx-in", txIn]
          ++ concat (replicate 24 ["--tx-out", txOut])
          ++ ["--out-file", unsignedFile]
    -- Sign those non-canonical body bytes. Re-encoding the body afterwards
    -- would change its hash and invalidate this sub-transaction signature.
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "sub-transaction"
        , "sign"
        , "--sub-tx-file"
        , unsignedFile
        , "--signing-key-file"
        , signingKey
        , "--out-file"
        , signedFile
        ]
    let buildArgs =
          [ "dijkstra"
          , "transaction"
          , "build-raw"
          , "--tx-in"
          , txIn
          , "--tx-out"
          , txOut
          , "--fee"
          , "200000"
          , "--signed-sub-tx-file"
          , signedFile
          ]
    -- Embedding the signed sub-transaction is allowed when the top-level
    -- output uses the default encoding and preserves the embedded body.
    void $ execCardanoCLI $ buildArgs ++ ["--out-file", bodyFile]
    -- Prepare a signed top-level transaction for the --tx-file signing path.
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "sign"
        , "--tx-body-file"
        , bodyFile
        , "--signing-key-file"
        , signingKey
        , "--out-file"
        , txFile
        ]
    -- Prepare a separate top-level witness for the assemble path. This signs
    -- the outer body; the embedded sub-transaction keeps its own signature.
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "witness"
        , "--tx-body-file"
        , bodyFile
        , "--signing-key-file"
        , signingKey
        , "--out-file"
        , witnessFile
        ]
    -- Each of these output paths can request canonicalisation of the whole
    -- top-level transaction, so each must protect the embedded signed body.
    forM_
      [ buildArgs
      , -- Sign an unsigned top-level body.
        ["dijkstra", "transaction", "sign", "--tx-body-file", bodyFile, "--signing-key-file", signingKey]
      , -- Add signatures to an already signed top-level transaction.
        ["dijkstra", "transaction", "sign", "--tx-file", txFile, "--signing-key-file", signingKey]
      , -- Assemble a top-level body with its separately created witness.
        ["dijkstra", "transaction", "assemble", "--tx-body-file", bodyFile, "--witness-file", witnessFile]
      ]
      $ \args -> do
        (exitCode, _, stderr) <-
          execDetailCardanoCLI $ args ++ ["--out-canonical-cbor", "--out-file", outFile]
        -- Fail with an actionable error, before writing an output whose
        -- embedded sub-transaction signature would no longer be valid.
        exitCode H.=== ExitFailure 1
        H.assertWith stderr ("invalidate its signatures" `isInfixOf`)
        H.assertWith stderr ("sub-transaction build-raw --out-canonical-cbor" `isInfixOf`)
        exists <- H.evalIO $ doesFileExist outFile
        exists H.=== False

-- | Canonicalise before signing, then batch, sign and assemble canonically.
hprop_dijkstra_canonical_sub_transaction_pipeline :: Property
hprop_dijkstra_canonical_sub_transaction_pipeline =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    unsignedFile <- H.noteTempFile tempDir "sub.unsigned"
    signedFile <- H.noteTempFile tempDir "sub.signed"
    bodyFile <- H.noteTempFile tempDir "tx.body"
    txFile <- H.noteTempFile tempDir "tx.signed"
    witnessFile <- H.noteTempFile tempDir "tx.witness"
    outFile <- H.noteTempFile tempDir "tx.assembled"
    -- Use the same 24-output body as the rejection test, but canonicalise
    -- it now, before any sub-transaction signature is created.
    void $
      execCardanoCLI $
        ["dijkstra", "transaction", "sub-transaction", "build-raw", "--tx-in", txIn]
          ++ concat (replicate 24 ["--tx-out", txOut])
          ++ ["--out-canonical-cbor", "--out-file", unsignedFile]
    -- Sign the canonical sub-transaction body. Later top-level output must
    -- preserve these bytes, because they determine the hash being signed.
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "sub-transaction"
        , "sign"
        , "--sub-tx-file"
        , unsignedFile
        , "--signing-key-file"
        , signingKey
        , "--out-file"
        , signedFile
        ]
    -- Adding witnesses must not change the sub-transaction body or its id.
    unsignedId <-
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "sub-transaction"
        , "txid"
        , "--sub-tx-file"
        , unsignedFile
        , "--output-text"
        ]
    signedId <-
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "sub-transaction"
        , "txid"
        , "--signed-sub-tx-file"
        , signedFile
        , "--output-text"
        ]
    unsignedId H.=== signedId
    -- Capture the canonical body bytes as our reference. After each outer
    -- transaction operation, check that these exact bytes remain embedded
    -- in its CBOR, preserving the hash signed by the sub-transaction witness.
    Exp.UnsignedSubTx unsigned <- H.evalEither =<< H.evalIO (readFileTextEnvelope (File unsignedFile))
    let bodyBytes = L.serialize' (Exp.eraProtVerHigh Exp.DijkstraEra) (unsigned ^. L.bodyTxL)
        checkEmbeddedBody path = do
          tx <-
            H.evalEither
              =<< H.evalIO
                (readFileTextEnvelope (File path) :: IO (Either (FileError TextEnvelopeError) (Tx DijkstraEra)))
          H.assert $ bodyBytes `BS.isInfixOf` serialiseToCBOR tx
    -- Embed the signed sub-transaction in a canonical top-level body. This
    -- succeeds because canonicalisation no longer changes the embedded body.
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "build-raw"
        , "--tx-in"
        , txIn
        , "--tx-out"
        , txOut
        , "--fee"
        , "200000"
        , "--signed-sub-tx-file"
        , signedFile
        , "--out-canonical-cbor"
        , "--out-file"
        , bodyFile
        ]
    checkEmbeddedBody bodyFile
    -- Sign the top-level body and request canonical output again. Its new
    -- witnesses must not alter the embedded sub-transaction body bytes.
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "sign"
        , "--tx-body-file"
        , bodyFile
        , "--signing-key-file"
        , signingKey
        , "--out-canonical-cbor"
        , "--out-file"
        , txFile
        ]
    checkEmbeddedBody txFile
    -- Exercise the other signing input: an already signed top-level tx.
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "sign"
        , "--tx-file"
        , txFile
        , "--signing-key-file"
        , signingKey
        , "--out-canonical-cbor"
        , "--out-file"
        , outFile
        ]
    checkEmbeddedBody outFile
    -- Exercise assembly independently: create an outer witness from the
    -- original top-level body, then combine them with canonical output.
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "witness"
        , "--tx-body-file"
        , bodyFile
        , "--signing-key-file"
        , signingKey
        , "--out-file"
        , witnessFile
        ]
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "assemble"
        , "--tx-body-file"
        , bodyFile
        , "--witness-file"
        , witnessFile
        , "--out-canonical-cbor"
        , "--out-file"
        , outFile
        ]
    checkEmbeddedBody outFile

-- | Sub-transactions reject Plutus V1-V3 script files for every script purpose,
-- before requiring protocol parameters or writing a body.
hprop_dijkstra_sub_transaction_rejects_older_plutus_scripts :: Property
hprop_dijkstra_sub_transaction_rejects_older_plutus_scripts =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    forM_ [1, 2, 3] $ \version ->
      forM_ (scriptFileCases (plutusScriptFile version)) $ \(purpose, witnessArgs) -> do
        H.note_ $ purpose ++ ": Plutus V" ++ show version
        (exitCode, _, stderr) <-
          execDetailCardanoCLI $ subTransactionArgs ++ witnessArgs ++ ["--out-file", outFile]
        exitCode H.=== ExitFailure 1
        H.assertWith stderr ("Sub-transactions require Plutus V4 for script execution" `isInfixOf`)
        H.assertWith stderr (("PlutusV" ++ show version) `isInfixOf`)
        exists <- H.evalIO $ doesFileExist outFile
        exists H.=== False

-- | Sub-transactions reject Plutus V1-V3 reference-script flags for every script purpose.
hprop_dijkstra_sub_transaction_rejects_older_plutus_reference_scripts :: Property
hprop_dijkstra_sub_transaction_rejects_older_plutus_reference_scripts =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    forM_ [1, 2, 3] $ \version ->
      forM_ (referenceScriptCases version) $ \(purpose, witnessArgs) -> do
        let flag = referenceLanguageFlag purpose version
        H.note_ flag
        (exitCode, _, stderr) <-
          execDetailCardanoCLI $ subTransactionArgs ++ witnessArgs ++ ["--out-file", outFile]
        exitCode H.=== ExitFailure 1
        H.assertWith stderr (("Invalid option `" ++ flag ++ "'") `isInfixOf`)
        exists <- H.evalIO $ doesFileExist outFile
        exists H.=== False

-- | A sub-transaction accepts a Plutus V4 script file as its spending witness.
hprop_dijkstra_sub_transaction_accepts_v4_script :: Property
hprop_dijkstra_sub_transaction_accepts_v4_script =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    paramsFile <- H.noteTempFile tempDir "protocol-params.json"
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    H.evalIO $ writeDijkstraProtocolParams paramsFile
    void $
      execCardanoCLI $
        subTransactionArgs
          ++ [ "--tx-in-script-file"
             , plutusScriptFile 4
             , "--tx-in-redeemer-value"
             , "0"
             , "--tx-in-execution-units"
             , "(0,0)"
             , "--protocol-params-file"
             , paramsFile
             , "--out-file"
             , outFile
             ]

-- | A sub-transaction accepts a native script as its spending witness.
hprop_dijkstra_sub_transaction_accepts_native_script :: Property
hprop_dijkstra_sub_transaction_accepts_native_script =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    void $
      execCardanoCLI $
        subTransactionArgs
          ++ [ "--tx-in-script-file"
             , "test/cardano-cli-test/files/input/shelley/multisig/scripts/all"
             , "--out-file"
             , outFile
             ]

-- | Storing an older reference script in an output does not execute it.
hprop_dijkstra_sub_transaction_can_store_v3_reference_script :: Property
hprop_dijkstra_sub_transaction_can_store_v3_reference_script =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    void $
      execCardanoCLI $
        subTransactionArgs
          ++ [ "--tx-out"
             , txOut
             , "--tx-out-reference-script-file"
             , plutusScriptFile 3
             , "--out-file"
             , outFile
             ]

-- | A native guard includes its script without requiring a Plutus redeemer or protocol parameters.
hprop_dijkstra_sub_transaction_native_guard :: Property
hprop_dijkstra_sub_transaction_native_guard =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    void $
      execCardanoCLI $
        subTransactionArgs ++ ["--guard-script-file", nativeScriptFile, "--out-file", outFile]
    Exp.UnsignedSubTx tx <- H.evalEither =<< H.evalIO (readFileTextEnvelope (File outFile))
    let scripts = tx ^. L.witsTxL . L.scriptTxWitsL
    length scripts H.=== 1
    toList (tx ^. L.bodyTxL . L.guardsTxBodyL) H.=== map L.ScriptHashObj (Map.keys scripts)
    L.unRedeemers (tx ^. L.witsTxL . L.rdmrsTxWitsL) H.=== mempty

-- | A Plutus guard requires both a redeemer and execution units; an incomplete pair fails too.
hprop_dijkstra_sub_transaction_guard_requires_redeemer_and_units :: Property
hprop_dijkstra_sub_transaction_guard_requires_redeemer_and_units =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    forM_ [[], ["--guard-redeemer-value", "0"], ["--guard-execution-units", "(0,0)"]] $ \args -> do
      (exitCode, _, stderr) <-
        execDetailCardanoCLI $
          subTransactionArgs ++ ["--guard-script-file", plutusScriptFile 4] ++ args ++ ["--out-file", outFile]
      exitCode H.=== ExitFailure 1
      H.assertWith stderr ("--guard-" `isInfixOf`)
      H.evalIO (doesFileExist outFile) >>= (H.=== False)

-- | Native scripts reject Plutus witness data, and key guards do not parse it.
hprop_dijkstra_sub_transaction_non_plutus_guard_rejects_redeemer :: Property
hprop_dijkstra_sub_transaction_non_plutus_guard_rejects_redeemer =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    forM_ [["--guard-script-file", nativeScriptFile], ["--guard-key-hash", replicate 56 '0']] $ \guard -> do
      (exitCode, _, _) <-
        execDetailCardanoCLI $
          subTransactionArgs
            ++ guard
            ++ ["--guard-redeemer-value", "0", "--guard-execution-units", "(0,0)", "--out-file", outFile]
      exitCode H.=== ExitFailure 1
      H.evalIO (doesFileExist outFile) >>= (H.=== False)

-- | Guard redeemers count key/native guards in their index and coexist with spending redeemers.
-- Changing the guarding redeemer must also change the body hash before signing.
hprop_dijkstra_sub_transaction_guard_redeemer_index :: Property
hprop_dijkstra_sub_transaction_guard_redeemer_index =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    paramsFile <- H.noteTempFile tempDir "protocol-params.json"
    firstFile <- H.noteTempFile tempDir "first.unsigned"
    secondFile <- H.noteTempFile tempDir "second.unsigned"
    H.evalIO $ writeDijkstraProtocolParams paramsFile
    forM_ [(firstFile, "42"), (secondFile, "43")] $ \(outFile, redeemer) ->
      void $
        execCardanoCLI $
          subTransactionArgs
            ++ [ "--tx-in-script-file"
               , plutusScriptFile 4
               , "--tx-in-redeemer-value"
               , "0"
               , "--tx-in-execution-units"
               , "(1,2)"
               , "--guard-key-hash"
               , replicate 56 '0'
               , "--guard-script-file"
               , nativeScriptFile
               , "--guard-script-file"
               , plutusScriptFile 4
               , "--guard-redeemer-value"
               , redeemer
               , "--guard-execution-units"
               , "(3,4)"
               , "--protocol-params-file"
               , paramsFile
               , "--out-file"
               , outFile
               ]
    first@(Exp.UnsignedSubTx tx) <- H.evalEither =<< H.evalIO (readFileTextEnvelope (File firstFile))
    second <- H.evalEither =<< H.evalIO (readFileTextEnvelope (File secondFile))
    let redeemers = L.unRedeemers $ tx ^. L.witsTxL . L.rdmrsTxWitsL
        datum = toAlonzoData . unsafeHashableScriptData . ScriptDataNumber
    Map.lookup (L.DijkstraSpending (L.AsIx 0)) redeemers
      H.=== Just (datum 0, toAlonzoExUnits $ ExecutionUnits 1 2)
    Map.lookup (L.DijkstraGuarding (L.AsIx 2)) redeemers
      H.=== Just (datum 42, toAlonzoExUnits $ ExecutionUnits 3 4)
    Map.size redeemers H.=== 2
    length (tx ^. L.bodyTxL . L.guardsTxBodyL) H.=== 3
    Map.size (tx ^. L.witsTxL . L.scriptTxWitsL) H.=== 2
    H.assert $ Exp.getUnsignedSubTxId first /= Exp.getUnsignedSubTxId second

-- | A hash guard referring to an already supplied Plutus script still needs its own redeemer.
hprop_dijkstra_sub_transaction_hash_guard_requires_own_redeemer :: Property
hprop_dijkstra_sub_transaction_hash_guard_requires_own_redeemer =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    paramsFile <- H.noteTempFile tempDir "protocol-params.json"
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    H.evalIO $ writeDijkstraProtocolParams paramsFile
    scriptHash <- execCardanoCLI ["hash", "script", "--script-file", plutusScriptFile 4]
    (exitCode, _, stderr) <-
      execDetailCardanoCLI $
        subTransactionArgs
          ++ [ "--tx-in-script-file"
             , plutusScriptFile 4
             , "--tx-in-redeemer-value"
             , "0"
             , "--tx-in-execution-units"
             , "(0,0)"
             , "--guard-script-hash"
             , filter (/= '\n') scriptHash
             , "--protocol-params-file"
             , paramsFile
             , "--out-file"
             , outFile
             ]
    exitCode H.=== ExitFailure 1
    H.assertWith stderr ("requires --guard-redeemer-* and --guard-execution-units" `isInfixOf`)

-- | A script file and its hash must not create two guards that collapse to one OSet entry.
hprop_dijkstra_sub_transaction_rejects_duplicate_guards :: Property
hprop_dijkstra_sub_transaction_rejects_duplicate_guards =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    scriptHash <-
      filter (/= '\n') <$> execCardanoCLI ["hash", "script", "--script-file", nativeScriptFile]
    (exitCode, _, stderr) <-
      execDetailCardanoCLI $
        subTransactionArgs
          ++ [ "--guard-script-file"
             , nativeScriptFile
             , "--guard-script-hash"
             , scriptHash
             , "--out-file"
             , outFile
             ]
    exitCode H.=== ExitFailure 1
    H.assertWith stderr ("Duplicate guards:" `isInfixOf`)
    H.assertWith stderr (scriptHash `isInfixOf`)

-- | Both Dijkstra builders accept V4 reference scripts for all six script purposes.
hprop_dijkstra_v4_reference_script_flags :: Property
hprop_dijkstra_v4_reference_script_flags =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    paramsFile <- H.noteTempFile tempDir "protocol-params.json"
    outFile <- H.noteTempFile tempDir "tx.body"
    H.evalIO $ writeDijkstraProtocolParams paramsFile
    forM_ (referenceScriptCases 4) $ \(purpose, witnessArgs) -> do
      H.note_ purpose
      let args = witnessArgs ++ ["--tx-out", txOut, "--protocol-params-file", paramsFile, "--out-file", outFile]
      void $ execCardanoCLI $ subTransactionArgs ++ args
      void $ execCardanoCLI $ transactionArgs "dijkstra" ++ args

-- | Top-level Dijkstra transactions still accept V2/V3 reference scripts,
-- with governance witnesses starting at V3.
hprop_dijkstra_top_level_accepts_older_plutus_reference_scripts :: Property
hprop_dijkstra_top_level_accepts_older_plutus_reference_scripts =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    paramsFile <- H.noteTempFile tempDir "protocol-params.json"
    outFile <- H.noteTempFile tempDir "tx.body"
    H.evalIO $ writeDijkstraProtocolParams paramsFile
    let v2Cases =
          [ testCase
          | testCase@(purpose, _) <- referenceScriptCases 2
          , purpose `notElem` ["vote", "proposal"]
          ]
    forM_ (v2Cases ++ referenceScriptCases 3) $ \(purpose, witnessArgs) -> do
      H.note_ purpose
      void $
        execCardanoCLI $
          transactionArgs "dijkstra"
            ++ witnessArgs
            ++ ["--tx-out", txOut, "--protocol-params-file", paramsFile, "--out-file", outFile]

-- | Conway and latest reject V4 reference-script flags for every script purpose.
hprop_conway_rejects_v4_reference_script_flags :: Property
hprop_conway_rejects_v4_reference_script_flags =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "tx.body"
    forM_ ["conway", "latest"] $ \era ->
      forM_ (referenceScriptCases 4) $ \(purpose, witnessArgs) -> do
        let flag = referenceLanguageFlag purpose 4
        H.note_ $ era ++ ": " ++ flag
        (exitCode, _, stderr) <-
          execDetailCardanoCLI $
            transactionArgs era ++ witnessArgs ++ ["--tx-out", txOut, "--out-file", outFile]
        exitCode H.=== ExitFailure 1
        H.assertWith stderr (("Invalid option `" ++ flag ++ "'") `isInfixOf`)
        exists <- H.evalIO $ doesFileExist outFile
        exists H.=== False

subTransactionArgs :: [String]
subTransactionArgs = ["dijkstra", "transaction", "sub-transaction", "build-raw", "--tx-in", txIn]

transactionArgs :: String -> [String]
transactionArgs era = [era, "transaction", "build-raw", "--tx-in", txIn, "--fee", "200000"]

plutusScriptFile :: Int -> FilePath
plutusScriptFile version = "test/cardano-cli-test/files/input/plutus/v" ++ show version ++ "-always-succeeds.plutus"

writeDijkstraProtocolParams :: FilePath -> IO ()
writeDijkstraProtocolParams path =
  Aeson.encodeFile
    path
    (L.PParams L.emptyPParamsIdentity :: L.PParams (Exp.LedgerEra Exp.DijkstraEra))

scriptFileCases :: FilePath -> [(String, [String])]
scriptFileCases scriptFile =
  [ ("spending", witness "tx-in" ++ ["--tx-in-datum-value", "0"])
  , ("guard", witness "guard")
  , ("mint", ["--mint", "1 " ++ replicate 56 '0'] ++ witness "mint")
  ,
    ( "certificate"
    , [ "--certificate-file"
      , "test/cardano-cli-test/files/input/sub-transaction/drep-registration-script.json"
      ]
        ++ witness "certificate"
    )
  ,
    ( "withdrawal"
    , ["--withdrawal", "stake_test17qvxuvh64q9zdqgrjt76d42eclk5wgdxtnsun4808cwg0dqxv5r99+10000"]
        ++ witness "withdrawal"
    )
  ,
    ( "vote"
    , ["--vote-file", "test/cardano-cli-test/files/input/sub-transaction/vote.drep.json"]
        ++ witness "vote"
    )
  ,
    ( "proposal"
    , [ "--proposal-file"
      , "test/cardano-cli-test/files/input/sub-transaction/protocol-parameters-update.action"
      ]
        ++ witness "proposal"
    )
  ]
 where
  witness prefix =
    [ "--" ++ prefix ++ "-script-file"
    , scriptFile
    , "--" ++ prefix ++ "-redeemer-value"
    , "0"
    , "--" ++ prefix ++ "-execution-units"
    , "(0,0)"
    ]

referenceLanguageFlag :: String -> Int -> String
referenceLanguageFlag purpose version = "--" ++ purpose ++ "-plutus-script-v" ++ show version

-- | Construct the requested version directly, including the datum required by V2 spending.
referenceScriptCases :: Int -> [(String, [String])]
referenceScriptCases version =
  [ ("spending", witness "spending" ++ spendingDatum)
  , ("mint", ["--mint", "1 " ++ policy] ++ witness "mint" ++ ["--policy-id", policy])
  ,
    ( "certificate"
    , [ "--certificate-file"
      , "test/cardano-cli-test/files/input/sub-transaction/drep-registration-script.json"
      ]
        ++ witness "certificate"
    )
  ,
    ( "withdrawal"
    , ["--withdrawal", "stake_test17qvxuvh64q9zdqgrjt76d42eclk5wgdxtnsun4808cwg0dqxv5r99+10000"]
        ++ witness "withdrawal"
    )
  ,
    ( "vote"
    , ["--vote-file", "test/cardano-cli-test/files/input/sub-transaction/vote.drep.json"]
        ++ witness "vote"
    )
  ,
    ( "proposal"
    , [ "--proposal-file"
      , "test/cardano-cli-test/files/input/sub-transaction/protocol-parameters-update.action"
      ]
        ++ witness "proposal"
    )
  ]
 where
  policy = replicate 56 '0'
  spendingDatum = if version == 2 then ["--spending-reference-tx-in-datum-value", "0"] else []
  witness purpose =
    [ "--" ++ purpose ++ "-tx-in-reference"
    , replicate 64 '0' ++ "#0"
    , referenceLanguageFlag purpose version
    , "--" ++ purpose ++ "-reference-tx-in-redeemer-value"
    , "0"
    , "--" ++ purpose ++ "-reference-tx-in-execution-units"
    , "(0,0)"
    ]
