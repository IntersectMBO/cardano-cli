{-# LANGUAGE DataKinds #-}

module Test.Cli.Transaction.SubTransaction where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Control.Monad (forM_, void)
import Data.ByteString qualified as BS
import Data.List (isInfixOf)
import Lens.Micro ((^.))
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H

signingKey :: FilePath
signingKey = "test/cardano-cli-test/files/input/sub-transaction/payment.skey"

txIn :: String
txIn = "63e6a9a8e58e48cc025cae04daaed9d36fc7b70bc292721d9f5057ae37b24981#0"

txOut :: String
txOut = "addr_test1vp0t4dfa9ktc2uvv7sg9leafuhtwyu0xcj4q4kf5pqkpjwqhklklg+5000000"

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
