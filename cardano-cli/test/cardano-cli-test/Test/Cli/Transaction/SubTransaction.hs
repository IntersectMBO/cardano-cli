{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cli.Transaction.SubTransaction where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Dijkstra.TxBody qualified as L
import Cardano.Ledger.Keys qualified as L
import Cardano.Ledger.Plutus.Language qualified as L

import Control.Monad (forM, forM_, void)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Foldable (toList)
import Data.List (isInfixOf, isPrefixOf)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
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

-- | Detached signing preserves every payload and agrees with direct signing,
-- including empty witness sets, multiple key roles and duplicate witnesses.
hprop_dijkstra_sub_transaction_witness_assemble :: Property
hprop_dijkstra_sub_transaction_witness_assemble =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    unsignedFile <- H.noteTempFile tempDir "sub.unsigned"
    signedFile <- H.noteTempFile tempDir "sub.signed"
    assembledFile <- H.noteTempFile tempDir "sub.assembled"
    paramsFile <- H.noteTempFile tempDir "params.json"
    metadataFile <- H.noteTempFile tempDir "metadata.json"
    stakeKeyFile <- H.noteTempFile tempDir "stake.skey"
    stakeVKeyFile <- H.noteTempFile tempDir "stake.vkey"
    H.evalIO $ writeDijkstraProtocolParams paramsFile
    H.evalIO $ writeFile metadataFile "{\"0\":42}"
    void $
      execCardanoCLI
        [ "dijkstra"
        , "stake-address"
        , "key-gen"
        , "--signing-key-file"
        , stakeKeyFile
        , "--verification-key-file"
        , stakeVKeyFile
        ]
    forM_ [False, True] $ \canonical -> do
      void $
        execCardanoCLI $
          subTransactionArgs
            ++ [ "--tx-in-script-file"
               , plutusScriptFile 4
               , "--tx-in-datum-value"
               , "0"
               , "--tx-in-redeemer-value"
               , "0"
               , "--tx-in-execution-units"
               , "(0,0)"
               , "--guard-script-file"
               , nativeScriptFile
               , "--metadata-json-file"
               , metadataFile
               , "--protocol-params-file"
               , paramsFile
               , "--out-file"
               , unsignedFile
               ]
            ++ concat (replicate 24 ["--tx-out", txOut])
            ++ ["--out-canonical-cbor" | canonical]
      unsigned@(Exp.UnsignedSubTx unsignedTx) <-
        H.evalEither =<< H.evalIO (readFileTextEnvelope (File unsignedFile))
      let bodyBytes = L.originalBytes (unsignedTx ^. L.bodyTxL)
      canonicalBytes <- H.evalEither $ canonicaliseCborBs bodyBytes
      (bodyBytes == canonicalBytes) H.=== canonical
      witnesses <- forM (zip ["payment", "stake"] [signingKey, stakeKeyFile]) $ \(name, key) -> do
        witnessFile <- H.noteTempFile tempDir (name ++ ".witness")
        void $
          execCardanoCLI
            [ "dijkstra"
            , "sub-transaction"
            , "witness"
            , "--sub-tx-file"
            , unsignedFile
            , "--signing-key-file"
            , key
            , "--out-file"
            , witnessFile
            ]
        pure witnessFile
      forM_ [0, 1, 2, 3] $ \count -> do
        let keys = take count [signingKey, stakeKeyFile, signingKey]
            files = take count (witnesses ++ witnesses)
        void $
          execCardanoCLI $
            ["dijkstra", "sub-transaction", "sign", "--sub-tx-file", unsignedFile, "--out-file", signedFile]
              ++ concatMap (\key -> ["--signing-key-file", key]) keys
        void $
          execCardanoCLI $
            [ "dijkstra"
            , "sub-transaction"
            , "assemble"
            , "--sub-tx-file"
            , unsignedFile
            , "--out-file"
            , assembledFile
            ]
              ++ concatMap (\file -> ["--witness-file", file]) files
        signedBytes <- H.evalIO $ BS.readFile signedFile
        assembledBytes <- H.evalIO $ BS.readFile assembledFile
        assembledBytes H.=== signedBytes
        assembled@(Exp.SignedSubTx assembledTx) <-
          H.evalEither =<< H.evalIO (readFileTextEnvelope (File assembledFile))
        Exp.getSignedSubTxId assembled H.=== Exp.getUnsignedSubTxId unsigned
        L.originalBytes (assembledTx ^. L.bodyTxL) H.=== bodyBytes
        assembledTx ^. L.auxDataTxL H.=== unsignedTx ^. L.auxDataTxL
        assembledTx ^. L.witsTxL . L.scriptTxWitsL H.=== unsignedTx ^. L.witsTxL . L.scriptTxWitsL
        assembledTx ^. L.witsTxL . L.datsTxWitsL H.=== unsignedTx ^. L.witsTxL . L.datsTxWitsL
        assembledTx ^. L.witsTxL . L.rdmrsTxWitsL H.=== unsignedTx ^. L.witsTxL . L.rdmrsTxWitsL
        Set.size (assembledTx ^. L.witsTxL . L.addrTxWitsL) H.=== min count 2

-- | Bad inputs fail before creating output; assembly accepts only Dijkstra witnesses.
hprop_dijkstra_sub_transaction_witness_assemble_rejects_invalid_inputs :: Property
hprop_dijkstra_sub_transaction_witness_assemble_rejects_invalid_inputs =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    unsignedFile <- H.noteTempFile tempDir "sub.unsigned"
    signedFile <- H.noteTempFile tempDir "sub.signed"
    malformedFile <- H.noteTempFile tempDir "malformed.json"
    missingFile <- H.noteTempFile tempDir "missing.json"
    bodyFile <- H.noteTempFile tempDir "conway.body"
    witnessFile <- H.noteTempFile tempDir "conway.witness"
    outFile <- H.noteTempFile tempDir "output"
    H.evalIO $ writeFile malformedFile "{}"
    void $ execCardanoCLI $ subTransactionArgs ++ ["--out-file", unsignedFile]
    void $
      execCardanoCLI
        ["dijkstra", "sub-transaction", "sign", "--sub-tx-file", unsignedFile, "--out-file", signedFile]
    void $
      execCardanoCLI
        ["conway", "transaction", "build-raw", "--tx-in", txIn, "--fee", "0", "--out-file", bodyFile]
    void $
      execCardanoCLI
        [ "conway"
        , "transaction"
        , "witness"
        , "--tx-body-file"
        , bodyFile
        , "--signing-key-file"
        , signingKey
        , "--out-file"
        , witnessFile
        ]
    let reject command args = do
          (exitCode, _, stderr) <-
            execDetailCardanoCLI $ ["dijkstra", "sub-transaction", command] ++ args ++ ["--out-file", outFile]
          exitCode H.=== ExitFailure 1
          H.evalIO (doesFileExist outFile) >>= (H.=== False)
          pure stderr
    stderr <- reject "assemble" ["--sub-tx-file", unsignedFile, "--witness-file", witnessFile]
    H.assertWith stderr ("The era of a witness does not match" `isInfixOf`)
    forM_ [signedFile, malformedFile, missingFile] $ \input -> do
      void $ reject "witness" ["--sub-tx-file", input, "--signing-key-file", signingKey]
      void $ reject "assemble" ["--sub-tx-file", input]
    forM_ [malformedFile, missingFile] $ \input -> do
      void $ reject "witness" ["--sub-tx-file", unsignedFile, "--signing-key-file", input]
      void $ reject "assemble" ["--sub-tx-file", unsignedFile, "--witness-file", input]
    void $ reject "witness" ["--sub-tx-file", unsignedFile]
    void $
      reject
        "witness"
        ["--sub-tx-file", unsignedFile, "--signing-key-file", signingKey, "--signing-key-file", signingKey]
    forM_ ["witness", "assemble"] $ \command -> do
      (exitCode, _, conwayError) <- execDetailCardanoCLI ["conway", "sub-transaction", command]
      exitCode H.=== ExitFailure 1
      H.assertWith conwayError ("Invalid argument `sub-transaction'" `isInfixOf`)

-- | Sub-transaction signing rejects the Byron-only address option at parse time.
hprop_dijkstra_sub_transaction_sign_rejects_address :: Property
hprop_dijkstra_sub_transaction_sign_rejects_address =
  watchdogProp . propertyOnce $ forM_ ["sign", "witness"] $ \command -> do
    (exitCode, _, stderr) <-
      execDetailCardanoCLI
        [ "dijkstra"
        , "sub-transaction"
        , command
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

-- | Duplicate body IDs are rejected, even across separate files with different witnesses.
hprop_dijkstra_build_raw_rejects_duplicate_sub_transaction :: Property
hprop_dijkstra_build_raw_rejects_duplicate_sub_transaction =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    unsignedFile <- H.noteTempFile tempDir "sub.unsigned"
    firstFile <- H.noteTempFile tempDir "first.signed"
    secondFile <- H.noteTempFile tempDir "second.signed"
    secondKey <- H.noteTempFile tempDir "second.skey"
    secondVKey <- H.noteTempFile tempDir "second.vkey"
    outFile <- H.noteTempFile tempDir "tx.body"
    void $
      execCardanoCLI
        ["address", "key-gen", "--signing-key-file", secondKey, "--verification-key-file", secondVKey]
    void $ execCardanoCLI $ subTransactionArgs ++ ["--out-file", unsignedFile]
    forM_ [(firstFile, signingKey), (secondFile, secondKey)] $ \(path, key) ->
      signSubTransaction unsignedFile key path
    first <- H.evalEither =<< H.evalIO (readFileTextEnvelope (File firstFile))
    second <- H.evalEither =<< H.evalIO (readFileTextEnvelope (File secondFile))
    let subTxId = Exp.getSignedSubTxId first
        expectedId = BSC.unpack $ serialiseToRawBytesHex subTxId
    subTxId H.=== Exp.getSignedSubTxId second
    H.assert $ serialiseToCBOR first /= serialiseToCBOR second
    forM_ [firstFile, secondFile] $ \duplicateFile -> do
      (exitCode, _, stderr) <-
        execDetailCardanoCLI $
          transactionArgs "dijkstra"
            ++ ["--signed-sub-tx-file", firstFile, "--signed-sub-tx-file", duplicateFile, "--out-file", outFile]
      exitCode H.=== ExitFailure 1
      H.assertWith stderr ("was provided more than once" `isInfixOf`)
      H.assertWith stderr (expectedId `isInfixOf`)
      H.evalIO (doesFileExist outFile) >>= (H.=== False)

-- | Embedding two different sub-transactions preserves their order, bodies and witnesses.
hprop_dijkstra_build_raw_embeds_two_sub_transactions :: Property
hprop_dijkstra_build_raw_embeds_two_sub_transactions =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "tx.body"
    subFiles <- forM ["1", "2"] $ \index -> do
      unsignedFile <- H.noteTempFile tempDir (index ++ ".unsigned")
      signedFile <- H.noteTempFile tempDir (index ++ ".signed")
      void $
        execCardanoCLI
          [ "dijkstra"
          , "sub-transaction"
          , "build-raw"
          , "--tx-in"
          , replicate 64 '0' ++ "#" ++ index
          , "--tx-out"
          , txOut
          , "--out-file"
          , unsignedFile
          ]
      signSubTransaction unsignedFile signingKey signedFile
      pure signedFile
    expected <- forM subFiles $ \path -> H.evalEither =<< H.evalIO (readFileTextEnvelope (File path))
    length (Set.fromList $ map Exp.getSignedSubTxId expected) H.=== 2
    void $
      execCardanoCLI $
        transactionArgs "dijkstra"
          ++ concatMap (\path -> ["--signed-sub-tx-file", path]) subFiles
          ++ ["--out-file", outFile]
    ShelleyTx _ tx <-
      readDijkstraTransaction outFile
    let actual = map Exp.SignedSubTx $ toList (tx ^. L.bodyTxL . L.subTransactionsTxBodyL)
    map serialiseToCBOR actual H.=== map serialiseToCBOR expected

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
    forM_ ["sign", "witness"] $ \command -> do
      (exitCode, _, stderr) <-
        execDetailCardanoCLI
          [ "dijkstra"
          , "sub-transaction"
          , command
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
      H.evalIO (doesFileExist outFile) >>= (H.=== False)
    -- A correctly encoded Dijkstra bootstrap witness must also be refused.
    bodyFile <- H.noteTempFile tempDir "tx.body"
    witnessFile <- H.noteTempFile tempDir "byron.witness"
    void $
      execCardanoCLI
        ["dijkstra", "transaction", "build-raw", "--tx-in", txIn, "--fee", "0", "--out-file", bodyFile]
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "witness"
        , "--tx-body-file"
        , bodyFile
        , "--signing-key-file"
        , byronKeyFile
        , "--testnet-magic"
        , "42"
        , "--out-file"
        , witnessFile
        ]
    (exitCode, _, stderr) <-
      execDetailCardanoCLI
        [ "dijkstra"
        , "sub-transaction"
        , "assemble"
        , "--sub-tx-file"
        , unsignedFile
        , "--witness-file"
        , witnessFile
        , "--out-file"
        , outFile
        ]
    exitCode H.=== ExitFailure 1
    H.assertWith
      stderr
      ("Byron (bootstrap) witnesses are not supported for sub-transactions" `isInfixOf`)
    H.evalIO (doesFileExist outFile) >>= (H.=== False)

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
        ["dijkstra", "sub-transaction", "build-raw", "--tx-in", txIn]
          ++ concat (replicate 24 ["--tx-out", txOut])
          ++ ["--out-file", unsignedFile]
    -- Sign those non-canonical body bytes. Re-encoding the body afterwards
    -- would change its hash and invalidate this sub-transaction signature.
    void $
      execCardanoCLI
        [ "dijkstra"
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
        ["dijkstra", "sub-transaction", "build-raw", "--tx-in", txIn]
          ++ concat (replicate 24 ["--tx-out", txOut])
          ++ ["--out-canonical-cbor", "--out-file", unsignedFile]
    -- Sign the canonical sub-transaction body. Later top-level output must
    -- preserve these bytes, because they determine the hash being signed.
    void $
      execCardanoCLI
        [ "dijkstra"
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
        , "sub-transaction"
        , "txid"
        , "--sub-tx-file"
        , unsignedFile
        , "--output-text"
        ]
    signedId <-
      execCardanoCLI
        [ "dijkstra"
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

-- | Canonical output must not reorder metadata after its auxiliary-data hash is calculated.
hprop_dijkstra_canonical_sub_transaction_rejects_metadata_rewrite :: Property
hprop_dijkstra_canonical_sub_transaction_rejects_metadata_rewrite =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    metadataFile <- H.noteTempFile tempDir "metadata.json"
    H.evalIO $ writeFile metadataFile $ "{\"0\":" ++ nonCanonicalDataMap ++ "}"
    assertCanonicalSubTxRejected
      tempDir
      "auxiliary data"
      ["--json-metadata-detailed-schema", "--metadata-json-file", metadataFile]

-- | Canonical output must preserve redeemer/datum bytes covered by script-integrity and datum hashes.
hprop_dijkstra_canonical_sub_transaction_rejects_script_data_rewrite :: Property
hprop_dijkstra_canonical_sub_transaction_rejects_script_data_rewrite =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    dataFile <- H.noteTempFile tempDir "data.json"
    paramsFile <- H.noteTempFile tempDir "protocol-params.json"
    H.evalIO $ writeFile dataFile nonCanonicalDataMap
    H.evalIO $ writeDijkstraProtocolParams paramsFile
    forM_
      [ ("redeemers", ["--tx-in-redeemer-file", dataFile])
      , ("datums", ["--tx-in-datum-file", dataFile, "--tx-in-redeemer-value", "0"])
      ]
      $ \(component, dataArgs) ->
        assertCanonicalSubTxRejected tempDir component $
          ["--tx-in-script-file", plutusScriptFile 4]
            ++ dataArgs
            ++ ["--tx-in-execution-units", "(0,0)", "--protocol-params-file", paramsFile]

-- | A native script's hash must not change when canonicalising its nested lists.
hprop_dijkstra_canonical_sub_transaction_rejects_native_script_rewrite :: Property
hprop_dijkstra_canonical_sub_transaction_rejects_native_script_rewrite =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    scriptFile <- H.noteTempFile tempDir "native.json"
    let signature = Aeson.object ["type" Aeson..= ("sig" :: String), "keyHash" Aeson..= replicate 56 '0']
    H.evalIO $
      Aeson.encodeFile scriptFile $
        Aeson.object ["type" Aeson..= ("all" :: String), "scripts" Aeson..= replicate 24 signature]
    assertCanonicalSubTxRejected tempDir "native script" ["--tx-in-script-file", scriptFile]

-- | Already canonical metadata and script data remain usable, with both stored hashes intact.
hprop_dijkstra_canonical_sub_transaction_preserves_payload_hashes :: Property
hprop_dijkstra_canonical_sub_transaction_preserves_payload_hashes =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    metadataFile <- H.noteTempFile tempDir "metadata.json"
    paramsFile <- H.noteTempFile tempDir "protocol-params.json"
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    H.evalIO $ writeFile metadataFile "{\"0\":42}"
    H.evalIO $ writeDijkstraProtocolParams paramsFile
    void $
      execCardanoCLI $
        subTransactionArgs
          ++ [ "--tx-in-script-file"
             , plutusScriptFile 4
             , "--tx-in-datum-value"
             , "0"
             , "--tx-in-redeemer-value"
             , "0"
             , "--tx-in-execution-units"
             , "(0,0)"
             , "--metadata-json-file"
             , metadataFile
             , "--protocol-params-file"
             , paramsFile
             , "--out-canonical-cbor"
             , "--out-file"
             , outFile
             ]
    unsigned@(Exp.UnsignedSubTx tx) <- H.evalEither =<< H.evalIO (readFileTextEnvelope (File outFile))
    canonical <- H.evalEither $ canonicaliseCborBs $ serialiseToCBOR unsigned
    canonical H.=== serialiseToCBOR unsigned
    L.SJust aux <- pure $ tx ^. L.auxDataTxL
    tx ^. L.bodyTxL . L.auxDataHashTxBodyL H.=== L.SJust (L.hashTxAuxData aux)
    let expectedIntegrity =
          getScriptIntegrityHash
            (BuildTxWith $ Just $ LedgerProtocolParameters $ L.PParams L.emptyPParamsIdentity)
            (Set.singleton L.PlutusV4)
            ( TxBodyScriptData
                AlonzoEraOnwardsDijkstra
                (tx ^. L.witsTxL . L.datsTxWitsL)
                (tx ^. L.witsTxL . L.rdmrsTxWitsL)
            )
    tx ^. L.bodyTxL . L.scriptIntegrityHashTxBodyL H.=== expectedIntegrity

-- Metadata and Plutus data both support this detailed-schema map. Its key order
-- changes under canonicalisation, so it exposes hashing-before-rewriting bugs.
nonCanonicalDataMap :: String
nonCanonicalDataMap = "{\"map\":[{\"k\":{\"int\":24},\"v\":{\"int\":0}},{\"k\":{\"int\":1},\"v\":{\"int\":0}}]}"

-- Check that canonical output rejects payload rewrites that would invalidate
-- hashes, both when building a sub-transaction and when embedding a signed one.
assertCanonicalSubTxRejected :: FilePath -> String -> [String] -> H.PropertyT (ResourceT IO) ()
assertCanonicalSubTxRejected tempDir component extraArgs = do
  unsignedFile <- H.noteTempFile tempDir "sub.unsigned"
  signedFile <- H.noteTempFile tempDir "sub.signed"
  bodyFile <- H.noteTempFile tempDir "tx.body"
  txFile <- H.noteTempFile tempDir "tx.signed"
  witnessFile <- H.noteTempFile tempDir "tx.witness"
  outFile <- H.noteTempFile tempDir "canonical.out"
  let buildSub = subTransactionArgs ++ extraArgs
      -- Require the payload-hash error and leave no output file.
      reject args = do
        (exitCode, _, stderr) <-
          execDetailCardanoCLI $ args ++ ["--out-canonical-cbor", "--out-file", outFile]
        exitCode H.=== ExitFailure 1
        H.assertWith stderr (component `isInfixOf`)
        H.assertWith stderr ("invalidate an existing hash" `isInfixOf`)
        H.evalIO (doesFileExist outFile) >>= (H.=== False)
  -- Ordinary output succeeds; canonical output must reject the same payload.
  void $ execCardanoCLI $ buildSub ++ ["--out-file", unsignedFile]
  reject buildSub
  -- Rule out body reordering as the reason for rejection.
  Exp.UnsignedSubTx subTx <- H.evalEither =<< H.evalIO (readFileTextEnvelope (File unsignedFile))
  let bodyBytes = L.originalBytes (subTx ^. L.bodyTxL)
  canonicalBody <- H.evalEither $ canonicaliseCborBs bodyBytes
  bodyBytes H.=== canonicalBody
  -- Sign and embed the sub-transaction, then prepare each outer output path.
  void $
    execCardanoCLI
      [ "dijkstra"
      , "sub-transaction"
      , "sign"
      , "--sub-tx-file"
      , unsignedFile
      , "--signing-key-file"
      , signingKey
      , "--out-file"
      , signedFile
      ]
  let buildOuter = transactionArgs "dijkstra" ++ ["--signed-sub-tx-file", signedFile]
  void $ execCardanoCLI $ buildOuter ++ ["--out-file", bodyFile]
  let signBody = ["dijkstra", "transaction", "sign", "--tx-body-file", bodyFile, "--signing-key-file", signingKey]
  void $ execCardanoCLI $ signBody ++ ["--out-file", txFile]
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
  -- Building, signing and assembling must all reject canonical payload rewrites.
  forM_
    [ buildOuter
    , signBody
    , ["dijkstra", "transaction", "sign", "--tx-file", txFile, "--signing-key-file", signingKey]
    , ["dijkstra", "transaction", "assemble", "--tx-body-file", bodyFile, "--witness-file", witnessFile]
    ]
    reject

-- | Sub-transactions reject Plutus V1-V3 script files for every script purpose,
-- before requiring protocol parameters or writing a body.
hprop_dijkstra_sub_transaction_rejects_older_plutus_scripts :: Property
hprop_dijkstra_sub_transaction_rejects_older_plutus_scripts =
  -- Allow time for 21 CLI invocations under Wine.
  propertyOnce . H.runWithWatchdog_ H.WatchdogConfig{H.watchdogTimeout = 90} $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    forM_ [1, 2, 3] $ \version ->
      forM_ (scriptFileCases (plutusScriptFile version) (replicate 56 '0')) $ \(purpose, witnessArgs) -> do
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
  -- Allow time for 18 CLI invocations under Wine.
  propertyOnce . H.runWithWatchdog_ H.WatchdogConfig{H.watchdogTimeout = 90} $ H.moduleWorkspace "tmp" $ \tempDir -> do
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

-- | Every script purpose retains the V4 script, redeemer and execution units in its output.
hprop_dijkstra_sub_transaction_accepts_v4_script :: Property
hprop_dijkstra_sub_transaction_accepts_v4_script =
  -- Allow time to build and inspect all seven script purposes under Wine.
  propertyOnce . H.runWithWatchdog_ H.WatchdogConfig{H.watchdogTimeout = 90} $ H.moduleWorkspace "tmp" $ \tempDir -> do
    paramsFile <- H.noteTempFile tempDir "protocol-params.json"
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    H.evalIO $ writeDijkstraProtocolParams paramsFile
    script <- H.evalEither =<< H.evalIO (readFileTextEnvelope (File (plutusScriptFile 4)))
    let expectedScript = toShelleyScript $ ScriptInEra PlutusScriptV4InDijkstra (PlutusScript PlutusScriptV4 script)
        scriptHash = L.hashScript expectedScript
        policyId = BSC.unpack $ serialiseToRawBytesHex $ fromShelleyScriptHash scriptHash
    forM_ (scriptFileCases (plutusScriptFile 4) policyId) $ \(purpose, witnessArgs) -> do
      H.note_ purpose
      void $
        execCardanoCLI $
          subTransactionArgs ++ witnessArgs ++ ["--protocol-params-file", paramsFile, "--out-file", outFile]
      Exp.UnsignedSubTx tx <- H.evalEither =<< H.evalIO (readFileTextEnvelope (File outFile))
      tx ^. L.witsTxL . L.scriptTxWitsL H.=== Map.singleton scriptHash expectedScript
      assertV4Redeemer purpose tx

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

-- | Dijkstra JSON witnesses may contain RequireGuard, including inside timelocks.
hprop_dijkstra_sub_transaction_accepts_require_guard_script :: Property
hprop_dijkstra_sub_transaction_accepts_require_guard_script =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    scriptFile <- H.noteTempFile tempDir "guard.json"
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    keyHash@(PaymentKeyHash ledgerHash) <-
      H.evalEither $ deserialiseFromRawBytesHex (BSC.replicate 56 '0')
    let guardScript = RequireGuard (L.KeyHashObj $ L.coerceKeyRole ledgerHash)
    forM_ [guardScript, RequireAllOf [guardScript, RequireTimeAfter 0]] $ \script -> do
      H.evalIO $ Aeson.encodeFile scriptFile script
      let expectedScript = toShelleyScript $ ScriptInEra SimpleScriptInDijkstra (SimpleScript script)
          scriptHash = L.hashScript expectedScript
          policyId = BSC.unpack $ serialiseToRawBytesHex $ fromShelleyScriptHash scriptHash
      forM_
        [ ["--tx-in-script-file", scriptFile]
        , ["--mint", "1 " ++ policyId, "--mint-script-file", scriptFile]
        ]
        $ \witnessArgs -> do
          H.noteShow_ witnessArgs
          void $
            execCardanoCLI $
              subTransactionArgs
                ++ witnessArgs
                ++ ["--guard-key-hash", BSC.unpack $ serialiseToRawBytesHex keyHash, "--out-file", outFile]
          Exp.UnsignedSubTx tx <- H.evalEither =<< H.evalIO (readFileTextEnvelope (File outFile))
          tx ^. L.witsTxL . L.scriptTxWitsL H.=== Map.singleton scriptHash expectedScript

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

-- | Key hashes, inline verification keys and key files produce the same key guard.
hprop_dijkstra_sub_transaction_key_guard_forms :: Property
hprop_dijkstra_sub_transaction_key_guard_forms =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    keyFile <- H.noteTempFile tempDir "payment.vkey"
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    key <- getVerificationKey <$> H.evalIO (generateSigningKey AsPaymentKey)
    H.evalEither =<< H.evalIO (writeFileTextEnvelope (File keyFile) Nothing key)
    let keyHash@(PaymentKeyHash ledgerHash) = verificationKeyHash key
        expectedGuard = L.KeyHashObj $ L.coerceKeyRole ledgerHash
    forM_
      [ ["--guard-key-hash", BSC.unpack $ serialiseToRawBytesHex keyHash]
      , ["--guard-verification-key", BSC.unpack $ serialiseToRawBytesHex key]
      , ["--guard-verification-key-file", keyFile]
      ]
      $ \guardArgs -> do
        H.noteShow_ guardArgs
        void $ execCardanoCLI $ subTransactionArgs ++ guardArgs ++ ["--out-file", outFile]
        Exp.UnsignedSubTx tx <- H.evalEither =<< H.evalIO (readFileTextEnvelope (File outFile))
        toList (tx ^. L.bodyTxL . L.guardsTxBodyL) H.=== [expectedGuard]

-- | A script-hash guard records its credential without adding a script or redeemer.
hprop_dijkstra_sub_transaction_script_hash_guard :: Property
hprop_dijkstra_sub_transaction_script_hash_guard =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    let hashHex = BSC.replicate 56 '1'
    scriptHash <- H.evalEither $ deserialiseFromRawBytesHex hashHex
    void $
      execCardanoCLI $
        subTransactionArgs ++ ["--guard-script-hash", BSC.unpack hashHex, "--out-file", outFile]
    Exp.UnsignedSubTx tx <- H.evalEither =<< H.evalIO (readFileTextEnvelope (File outFile))
    toList (tx ^. L.bodyTxL . L.guardsTxBodyL) H.=== [L.ScriptHashObj $ toShelleyScriptHash scriptHash]
    tx ^. L.witsTxL . L.scriptTxWitsL H.=== mempty
    L.unRedeemers (tx ^. L.witsTxL . L.rdmrsTxWitsL) H.=== mempty

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

-- | Both builders retain V4 reference witnesses for all six script purposes.
hprop_dijkstra_v4_reference_script_flags :: Property
hprop_dijkstra_v4_reference_script_flags =
  -- Allow time for both builders across all six script purposes under Wine.
  propertyOnce . H.runWithWatchdog_ H.WatchdogConfig{H.watchdogTimeout = 90} $ H.moduleWorkspace "tmp" $ \tempDir -> do
    paramsFile <- H.noteTempFile tempDir "protocol-params.json"
    outFile <- H.noteTempFile tempDir "tx.body"
    H.evalIO $ writeDijkstraProtocolParams paramsFile
    forM_ (referenceScriptCases 4) $ \(purpose, witnessArgs) -> do
      H.note_ purpose
      let args = witnessArgs ++ ["--tx-out", txOut, "--protocol-params-file", paramsFile, "--out-file", outFile]
      void $ execCardanoCLI $ subTransactionArgs ++ args
      Exp.UnsignedSubTx subTx <- H.evalEither =<< H.evalIO (readFileTextEnvelope (File outFile))
      assertV4ReferenceWitness purpose subTx
      void $ execCardanoCLI $ transactionArgs "dijkstra" ++ args
      ShelleyTx _ tx <-
        readDijkstraTransaction outFile
      assertV4ReferenceWitness purpose tx

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

-- | Sign a sub-transaction with the supplied key.
signSubTransaction :: FilePath -> FilePath -> FilePath -> H.PropertyT (ResourceT IO) ()
signSubTransaction unsignedFile key signedFile =
  void $
    execCardanoCLI
      [ "dijkstra"
      , "sub-transaction"
      , "sign"
      , "--sub-tx-file"
      , unsignedFile
      , "--signing-key-file"
      , key
      , "--out-file"
      , signedFile
      ]

-- | Decode a top-level Dijkstra transaction for assertions on its contents.
readDijkstraTransaction :: FilePath -> H.PropertyT (ResourceT IO) (Tx DijkstraEra)
readDijkstraTransaction path =
  H.evalEither =<< H.evalIO (readFileTextEnvelope (File path))

-- | All key forms produce a datum-free key credential. A script hash with the
-- same bytes is a distinct credential and retains its own datum in either order.
hprop_dijkstra_sub_transaction_top_level_key_guard_forms :: Property
hprop_dijkstra_sub_transaction_top_level_key_guard_forms =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    (key, forms) <- topLevelKeyGuardFixture tempDir
    let keyHash@(PaymentKeyHash ledgerHash) = verificationKeyHash key
        hashHex = serialiseToRawBytesHex keyHash
        scriptArgs = ["--top-level-guard-script-hash", BSC.unpack hashHex, "--top-level-guard-datum-value", "42"]
    scriptHash <- H.evalEither $ deserialiseFromRawBytesHex hashHex
    let expected =
          Map.fromList
            [ (L.KeyHashObj $ L.coerceKeyRole ledgerHash, L.SNothing)
            , (L.ScriptHashObj $ toShelleyScriptHash scriptHash, L.SJust $ toAlonzoData guardDatum)
            ]
    forM_ forms $ \keyArgs ->
      forM_ [keyArgs ++ scriptArgs, scriptArgs ++ keyArgs] $ \args -> do
        H.noteShow_ args
        Exp.UnsignedSubTx tx <- buildSubTransaction outFile args
        tx ^. L.bodyTxL . L.requiredTopLevelGuardsL H.=== expected
        tx ^. L.bodyTxL . L.guardsTxBodyL H.=== mempty
        tx ^. L.witsTxL H.=== mempty

-- | Key requirements reject datums and duplicate credentials across all key forms.
hprop_dijkstra_sub_transaction_rejects_invalid_top_level_key_guards :: Property
hprop_dijkstra_sub_transaction_rejects_invalid_top_level_key_guards =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    (key, forms) <- topLevelKeyGuardFixture tempDir
    let hashHex = BSC.unpack $ serialiseToRawBytesHex $ verificationKeyHash key
    forM_ forms $ \args -> do
      stderr <- expectSubTransactionFailure outFile $ args ++ ["--top-level-guard-datum-value", "42"]
      H.assertWith stderr ("Missing:" `isInfixOf`)
      forM_ forms $ \duplicate -> do
        duplicateError <- expectSubTransactionFailure outFile $ args ++ duplicate
        H.assertWith duplicateError ("Duplicate top-level guards:" `isInfixOf`)
        H.assertWith duplicateError (hashHex `isInfixOf`)

-- | Multiple distinct script-hash guards retain their own optional datum.
-- A hash alone accepts either form because its script language is unknown.
hprop_dijkstra_sub_transaction_top_level_guard_hashes :: Property
hprop_dijkstra_sub_transaction_top_level_guard_hashes =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    Exp.UnsignedSubTx tx <-
      buildSubTransaction
        outFile
        (concatMap topLevelGuardHashArgs [('0', Nothing), ('1', Just 42), ('2', Nothing)])
    let datum = toAlonzoData guardDatum
    expected <- forM [('0', L.SNothing), ('1', L.SJust datum), ('2', L.SNothing)] $ \(digit, mDatum) -> do
      ScriptHash hash <- H.evalEither $ deserialiseFromRawBytesHex $ BSC.pack $ replicate 56 digit
      pure (L.ScriptHashObj hash, mDatum)
    tx ^. L.bodyTxL . L.requiredTopLevelGuardsL H.=== Map.fromList expected

-- | Inline, JSON-file and CBOR-file datums produce the same guard entry.
hprop_dijkstra_sub_transaction_top_level_guard_datum_formats :: Property
hprop_dijkstra_sub_transaction_top_level_guard_datum_formats =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    jsonFile <- H.noteTempFile tempDir "datum.json"
    cborFile <- H.noteTempFile tempDir "datum.cbor"
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    H.evalIO $ Aeson.encodeFile jsonFile $ scriptDataToJson ScriptDataJsonDetailedSchema guardDatum
    H.evalIO $ BS.writeFile cborFile $ serialiseToCBOR guardDatum
    let build datumArgs =
          buildSubTransaction outFile $
            ["--top-level-guard-script-hash", replicate 56 '0'] ++ datumArgs
    inline <- build ["--top-level-guard-datum-value", "42"]
    json <- build ["--top-level-guard-datum-file", jsonFile]
    cbor <- build ["--top-level-guard-datum-cbor-file", cborFile]
    inline H.=== json
    inline H.=== cbor
    ScriptHash hash <- H.evalEither $ deserialiseFromRawBytesHex (BSC.replicate 56 '0')
    let Exp.UnsignedSubTx tx = inline
    tx ^. L.bodyTxL . L.requiredTopLevelGuardsL
      H.=== Map.singleton (L.ScriptHashObj hash) (L.SJust $ toAlonzoData guardDatum)

-- | Script files resolve to the same guard entries as their hashes, for both
-- native scripts without datums and Plutus scripts with datums.
hprop_dijkstra_sub_transaction_top_level_guard_script_files :: Property
hprop_dijkstra_sub_transaction_top_level_guard_script_files =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    forM_
      [ (nativeScriptFile, [], L.SNothing)
      , (plutusScriptFile 4, ["--top-level-guard-datum-value", "42"], L.SJust $ toAlonzoData guardDatum)
      ]
      $ \(scriptFile, datumArgs, expectedDatum) -> do
        scriptHash <- filter (/= '\n') <$> execCardanoCLI ["hash", "script", "--script-file", scriptFile]
        fromFile <- buildSubTransaction outFile $ ["--top-level-guard-script-file", scriptFile] ++ datumArgs
        fromHash <- buildSubTransaction outFile $ ["--top-level-guard-script-hash", scriptHash] ++ datumArgs
        fromFile H.=== fromHash
        -- A top-level requirement contributes no local guard or witness data.
        let Exp.UnsignedSubTx tx = fromFile
        ScriptHash hash <- H.evalEither $ deserialiseFromRawBytesHex (BSC.pack scriptHash)
        tx ^. L.bodyTxL . L.requiredTopLevelGuardsL
          H.=== Map.singleton (L.ScriptHashObj hash) expectedDatum
        tx ^. L.bodyTxL . L.guardsTxBodyL H.=== mempty
        tx ^. L.witsTxL H.=== mempty

-- | The same script can guard both levels, with a local redeemer and a distinct
-- datum for the enclosing transaction. Adding the requirement preserves witnesses.
hprop_dijkstra_sub_transaction_top_level_guard_coexists_with_local_guard :: Property
hprop_dijkstra_sub_transaction_top_level_guard_coexists_with_local_guard =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    paramsFile <- H.noteTempFile tempDir "protocol-params.json"
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    H.evalIO $ writeDijkstraProtocolParams paramsFile
    let localGuardArgs =
          [ "--guard-script-file"
          , plutusScriptFile 4
          , "--guard-redeemer-value"
          , "43"
          , "--guard-execution-units"
          , fixtureExecutionUnitsArg
          , "--protocol-params-file"
          , paramsFile
          ]
    Exp.UnsignedSubTx local <- buildSubTransaction outFile localGuardArgs
    Exp.UnsignedSubTx combined <-
      buildSubTransaction outFile $
        localGuardArgs
          ++ [ "--top-level-guard-script-file"
             , plutusScriptFile 4
             , "--top-level-guard-datum-value"
             , "42"
             ]
    [credential] <- pure $ toList (local ^. L.bodyTxL . L.guardsTxBodyL)
    combined ^. L.bodyTxL . L.requiredTopLevelGuardsL
      H.=== Map.singleton credential (L.SJust $ toAlonzoData guardDatum)
    combined ^. L.bodyTxL . L.guardsTxBodyL H.=== local ^. L.bodyTxL . L.guardsTxBodyL
    combined ^. L.witsTxL H.=== local ^. L.witsTxL
    combined ^. L.bodyTxL . L.scriptIntegrityHashTxBodyL
      H.=== local ^. L.bodyTxL . L.scriptIntegrityHashTxBodyL

-- | Guard datums belong to the body: canonicalise them before signing, or
-- reject canonical outer output that would change the signed body bytes.
hprop_dijkstra_sub_transaction_top_level_guard_canonical_datum :: Property
hprop_dijkstra_sub_transaction_top_level_guard_canonical_datum =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    datumFile <- H.noteTempFile tempDir "datum.json"
    unsignedFile <- H.noteTempFile tempDir "sub.unsigned"
    signedFile <- H.noteTempFile tempDir "sub.signed"
    bodyFile <- H.noteTempFile tempDir "tx.body"
    canonicalFile <- H.noteTempFile tempDir "canonical.body"
    H.evalIO $ writeFile datumFile nonCanonicalDataMap
    forM_ [False, True] $ \canonical -> do
      let canonicalArgs = ["--out-canonical-cbor" | canonical]
      Exp.UnsignedSubTx unsigned <-
        buildSubTransaction unsignedFile $
          [ "--top-level-guard-script-file"
          , plutusScriptFile 4
          , "--top-level-guard-datum-file"
          , datumFile
          ]
            ++ canonicalArgs
      let bodyBytes = L.originalBytes (unsigned ^. L.bodyTxL)
      canonicalBytes <- H.evalEither $ canonicaliseCborBs bodyBytes
      (bodyBytes == canonicalBytes) H.=== canonical
      signSubTransaction unsignedFile signingKey signedFile
      Exp.SignedSubTx signed <- H.evalEither =<< H.evalIO (readFileTextEnvelope (File signedFile))
      L.originalBytes (signed ^. L.bodyTxL) H.=== bodyBytes

      -- Ordinary embedding preserves either encoding, including the signature.
      let buildOuter = transactionArgs "dijkstra" ++ ["--signed-sub-tx-file", signedFile]
          checkEmbedded path = do
            ShelleyTx _ outer <- readDijkstraTransaction path
            [embedded] <- pure $ toList (outer ^. L.bodyTxL . L.subTransactionsTxBodyL)
            serialiseToCBOR (Exp.SignedSubTx embedded) H.=== serialiseToCBOR (Exp.SignedSubTx signed)
      void $ execCardanoCLI $ buildOuter ++ ["--out-file", bodyFile]
      checkEmbedded bodyFile

      -- Canonical top-level output must preserve the signed sub-transaction bytes.
      let canonicalOuter = buildOuter ++ ["--out-canonical-cbor", "--out-file", canonicalFile]
      if canonical
        then do
          void $ execCardanoCLI canonicalOuter
          checkEmbedded canonicalFile
        else do
          (exitCode, _, stderr) <- execDetailCardanoCLI canonicalOuter
          exitCode H.=== ExitFailure 1
          H.assertWith stderr ("invalidate its signatures" `isInfixOf`)
          H.evalIO (doesFileExist canonicalFile) >>= (H.=== False)

-- | Plutus V1-V3 guard files are rejected even when a datum is supplied.
hprop_dijkstra_sub_transaction_top_level_guard_rejects_older_plutus_scripts :: Property
hprop_dijkstra_sub_transaction_top_level_guard_rejects_older_plutus_scripts =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    forM_ [1, 2, 3] $ \version -> do
      stderr <-
        expectSubTransactionFailure
          outFile
          [ "--top-level-guard-script-file"
          , plutusScriptFile version
          , "--top-level-guard-datum-value"
          , "42"
          ]
      H.assertWith
        stderr
        (("Top-level guard script " ++ plutusScriptFile version ++ " must be Plutus V4") `isInfixOf`)
      H.assertWith stderr (("PlutusV" ++ show version) `isInfixOf`)

-- | Known Plutus guards require a datum, and known native guards reject one.
hprop_dijkstra_sub_transaction_top_level_guard_invalid_datums :: Property
hprop_dijkstra_sub_transaction_top_level_guard_invalid_datums =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    forM_
      [ (plutusScriptFile 4, [], "requires a datum")
      , (nativeScriptFile, ["--top-level-guard-datum-value", "42"], "must not have a datum")
      ]
      $ \(scriptFile, datumArgs, expectedError) -> do
        stderr <-
          expectSubTransactionFailure outFile $ ["--top-level-guard-script-file", scriptFile] ++ datumArgs
        H.assertWith stderr (expectedError `isInfixOf`)

-- | Malformed hashes and datums, and datums without a guard, fail to parse.
hprop_dijkstra_sub_transaction_top_level_guard_parser_failures :: Property
hprop_dijkstra_sub_transaction_top_level_guard_parser_failures =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    forM_
      [
        ( ["--top-level-guard-script-hash", "invalid"]
        , ["option --top-level-guard-script-hash:", "expecting hexadecimal digit"]
        )
      ,
        ( ["--top-level-guard-key-hash", "invalid"]
        , ["option --top-level-guard-key-hash:", "expecting hexadecimal digit"]
        )
      , (["--top-level-guard-verification-key", "invalid"], ["Invalid guard verification key"])
      ,
        ( ["--top-level-guard-datum-value", "42"]
        , ["Missing:", "--top-level-guard-script-hash HASH | --top-level-guard-script-file FILEPATH"]
        )
      ,
        ( ["--top-level-guard-script-hash", replicate 56 '0', "--top-level-guard-datum-value", "{"]
        , ["option --top-level-guard-datum-value:", "Unexpected end-of-input"]
        )
      ]
      $ \(args, expectedErrors) -> do
        stderr <- expectSubTransactionFailure outFile args
        -- Match the diagnostic, not option names repeated in the usage text.
        let diagnostic = unlines $ takeWhile (not . isPrefixOf "Usage:") (lines stderr)
        forM_ expectedErrors $ \message -> H.assertWith diagnostic (message `isInfixOf`)

-- | Report every duplicate hash, including guards without datums and guards
-- with conflicting datums, while excluding credentials given only once.
hprop_dijkstra_sub_transaction_rejects_duplicate_top_level_guard_hashes :: Property
hprop_dijkstra_sub_transaction_rejects_duplicate_top_level_guard_hashes =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    stderr <-
      expectSubTransactionFailure
        outFile
        ( concatMap
            topLevelGuardHashArgs
            [('0', Nothing), ('1', Just 42), ('2', Nothing), ('0', Nothing), ('1', Just 43)]
        )
    H.assertWith stderr ("Duplicate top-level guards:" `isInfixOf`)
    forM_ ['0', '1'] $ \digit -> H.assertWith stderr (replicate 56 digit `isInfixOf`)
    H.assertWith stderr (not . isInfixOf (replicate 56 '2'))

-- | Repeated files and file/hash aliases are duplicates, even with matching
-- datums. A hash entry must not erase the datum checked for a Plutus file.
hprop_dijkstra_sub_transaction_rejects_duplicate_top_level_guard_files :: Property
hprop_dijkstra_sub_transaction_rejects_duplicate_top_level_guard_files =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "sub.unsigned"
    forM_
      [ (nativeScriptFile, [])
      , (plutusScriptFile 4, ["--top-level-guard-datum-value", "42"])
      ]
      $ \(scriptFile, datumArgs) -> do
        scriptHash <- filter (/= '\n') <$> execCardanoCLI ["hash", "script", "--script-file", scriptFile]
        let fileArgs = ["--top-level-guard-script-file", scriptFile] ++ datumArgs
            hashArgs = ["--top-level-guard-script-hash", scriptHash]
        forM_ [fileArgs ++ hashArgs, hashArgs ++ fileArgs, fileArgs ++ fileArgs] $ \args -> do
          stderr <- expectSubTransactionFailure outFile args
          H.assertWith stderr ("Duplicate top-level guards:" `isInfixOf`)
          H.assertWith stderr (scriptHash `isInfixOf`)

-- | A key and the three CLI forms that must resolve to the same credential.
topLevelKeyGuardFixture
  :: FilePath -> H.PropertyT (ResourceT IO) (VerificationKey PaymentKey, [[String]])
topLevelKeyGuardFixture tempDir = do
  keyFile <- H.noteTempFile tempDir "payment.vkey"
  key <- getVerificationKey <$> H.evalIO (generateSigningKey AsPaymentKey)
  H.evalEither =<< H.evalIO (writeFileTextEnvelope (File keyFile) Nothing key)
  pure
    ( key
    ,
      [ ["--top-level-guard-key-hash", BSC.unpack $ serialiseToRawBytesHex $ verificationKeyHash key]
      , ["--top-level-guard-verification-key", BSC.unpack $ serialiseToRawBytesHex key]
      , ["--top-level-guard-verification-key-file", keyFile]
      ]
    )

topLevelGuardHashArgs :: (Char, Maybe Integer) -> [String]
topLevelGuardHashArgs (digit, datum) =
  ["--top-level-guard-script-hash", replicate 56 digit]
    ++ maybe [] (\value -> ["--top-level-guard-datum-value", show value]) datum

buildSubTransaction
  :: FilePath -> [String] -> H.PropertyT (ResourceT IO) Exp.UnsignedSubTx
buildSubTransaction outFile args = do
  void $ execCardanoCLI $ subTransactionArgs ++ args ++ ["--out-file", outFile]
  H.evalEither =<< H.evalIO (readFileTextEnvelope (File outFile))

expectSubTransactionFailure
  :: FilePath -> [String] -> H.PropertyT (ResourceT IO) String
expectSubTransactionFailure outFile args = do
  (exitCode, _, stderr) <-
    execDetailCardanoCLI $ subTransactionArgs ++ args ++ ["--out-file", outFile]
  exitCode H.=== ExitFailure 1
  exists <- H.evalIO $ doesFileExist outFile
  exists H.=== False
  pure stderr

guardDatum :: HashableScriptData
guardDatum = unsafeHashableScriptData $ ScriptDataNumber 42

subTransactionArgs :: [String]
subTransactionArgs = ["dijkstra", "sub-transaction", "build-raw", "--tx-in", txIn]

transactionArgs :: String -> [String]
transactionArgs era = [era, "transaction", "build-raw", "--tx-in", txIn, "--fee", "200000"]

plutusScriptFile :: Int -> FilePath
plutusScriptFile version = "test/cardano-cli-test/files/input/plutus/v" ++ show version ++ "-always-succeeds.plutus"

writeDijkstraProtocolParams :: FilePath -> IO ()
writeDijkstraProtocolParams path =
  Aeson.encodeFile
    path
    (L.PParams L.emptyPParamsIdentity :: L.PParams (Exp.LedgerEra Exp.DijkstraEra))

-- Arbitrary budget shared by the fixture arguments and output assertions.
-- build-raw records these units without evaluating the script.
fixtureExecutionUnits :: ExecutionUnits
fixtureExecutionUnits = ExecutionUnits 7 11

fixtureExecutionUnitsArg :: String
fixtureExecutionUnitsArg = case fixtureExecutionUnits of
  ExecutionUnits steps memory -> show (steps, memory)

scriptFileCases :: FilePath -> String -> [(String, [String])]
scriptFileCases scriptFile policy =
  [ ("spending", witness "tx-in" ++ ["--tx-in-datum-value", "0"])
  , ("guard", witness "guard")
  , ("mint", ["--mint", "1 " ++ policy] ++ witness "mint")
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
    , "42"
    , "--" ++ prefix ++ "-execution-units"
    , fixtureExecutionUnitsArg
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
    , "42"
    , "--" ++ purpose ++ "-reference-tx-in-execution-units"
    , fixtureExecutionUnitsArg
    ]

-- Reference-script language is not stored in the witness set: it contributes
-- to the integrity hash. Check that hash as well as the input and redeemer.
assertV4ReferenceWitness
  :: String -> L.Tx level (Exp.LedgerEra Exp.DijkstraEra) -> H.PropertyT (ResourceT IO) ()
assertV4ReferenceWitness purpose tx = do
  referenceId <- H.evalEither $ deserialiseFromRawBytesHex (BSC.replicate 64 '0')
  tx ^. L.bodyTxL . L.referenceInputsTxBodyL
    H.=== Set.singleton (toShelleyTxIn $ TxIn referenceId (TxIx 0))
  tx ^. L.witsTxL . L.scriptTxWitsL H.=== mempty
  assertV4Redeemer purpose tx

-- Each fixture has one witness at index zero. Check the entire redeemer map
-- and both language hashes so selecting V3 or dropping a witness cannot pass.
assertV4Redeemer
  :: String -> L.Tx level (Exp.LedgerEra Exp.DijkstraEra) -> H.PropertyT (ResourceT IO) ()
assertV4Redeemer purpose tx = do
  pointer <-
    H.evalMaybe $
      lookup
        purpose
        [ ("spending", L.DijkstraSpending (L.AsIx 0))
        , ("mint", L.DijkstraMinting (L.AsIx 0))
        , ("certificate", L.DijkstraCertifying (L.AsIx 0))
        , ("withdrawal", L.DijkstraWithdrawing (L.AsIx 0))
        , ("vote", L.DijkstraVoting (L.AsIx 0))
        , ("proposal", L.DijkstraProposing (L.AsIx 0))
        , ("guard", L.DijkstraGuarding (L.AsIx 0))
        ]
  let redeemers = tx ^. L.witsTxL . L.rdmrsTxWitsL
      redeemer = toAlonzoData $ unsafeHashableScriptData $ ScriptDataNumber 42
      executionUnits = toAlonzoExUnits fixtureExecutionUnits
      integrityHash language =
        getScriptIntegrityHash
          (BuildTxWith $ Just $ LedgerProtocolParameters $ L.PParams L.emptyPParamsIdentity)
          (Set.singleton language)
          (TxBodyScriptData AlonzoEraOnwardsDijkstra (tx ^. L.witsTxL . L.datsTxWitsL) redeemers)
  L.unRedeemers redeemers H.=== Map.singleton pointer (redeemer, executionUnits)
  tx ^. L.bodyTxL . L.scriptIntegrityHashTxBodyL H.=== integrityHash L.PlutusV4
  H.assert $ integrityHash L.PlutusV4 /= integrityHash L.PlutusV3
