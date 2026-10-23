module Test.Golden.Dijkstra.Transaction.SubTransaction where

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog (Property, (===))
import Hedgehog.Extras.Test qualified as H

-- | The fixture signing key and the hash of its verification key, used as a
-- guard so the sub-transaction demands that key's signature.
signingKey :: FilePath
signingKey = "test/cardano-cli-golden/files/input/dijkstra/keys/utxo_keys/signing_key"

signingKeyHash :: String
signingKeyHash = "944b17df58cf59ca727d08746addbc5cb55cd87151ba69decd35bbbc"

goldenDir :: FilePath
goldenDir = "test/cardano-cli-golden/files/golden/dijkstra/transaction/sub-transaction"

-- | Compare every transaction file and ID in the build/sign/embed pipeline with its golden output.
hprop_golden_dijkstra_transaction_sub_transaction_pipeline :: Property
hprop_golden_dijkstra_transaction_sub_transaction_pipeline =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    signingKeyFile <- noteInputFile signingKey

    -- Build the unsigned sub-transaction
    unsignedSubTxFile <- noteTempFile tempDir "sub-tx.unsigned"
    void $
      execCardanoCLI
        [ "dijkstra"
        , "sub-transaction"
        , "build-raw"
        , "--tx-in"
        , "63e6a9a8e58e48cc025cae04daaed9d36fc7b70bc292721d9f5057ae37b24981#1"
        , "--tx-out"
        , "addr_test1vp0t4dfa9ktc2uvv7sg9leafuhtwyu0xcj4q4kf5pqkpjwqhklklg+5000000"
        , "--invalid-hereafter"
        , "99999"
        , "--guard-key-hash"
        , signingKeyHash
        , "--out-file"
        , unsignedSubTxFile
        ]
    H.diffFileVsGoldenFile unsignedSubTxFile (goldenDir <> "/unsigned_out")

    -- Sign it
    signedSubTxFile <- noteTempFile tempDir "sub-tx.signed"
    void $
      execCardanoCLI
        [ "dijkstra"
        , "sub-transaction"
        , "sign"
        , "--sub-tx-file"
        , unsignedSubTxFile
        , "--signing-key-file"
        , signingKeyFile
        , "--out-file"
        , signedSubTxFile
        ]
    H.diffFileVsGoldenFile signedSubTxFile (goldenDir <> "/signed_out")

    -- Creating a detached witness and assembling must reproduce direct signing.
    witnessFile <- noteTempFile tempDir "sub-tx.witness"
    void $
      execCardanoCLI
        [ "dijkstra"
        , "sub-transaction"
        , "witness"
        , "--sub-tx-file"
        , unsignedSubTxFile
        , "--signing-key-file"
        , signingKeyFile
        , "--out-file"
        , witnessFile
        ]
    H.diffFileVsGoldenFile witnessFile (goldenDir <> "/witness_out")
    void $
      execCardanoCLI
        [ "dijkstra"
        , "sub-transaction"
        , "assemble"
        , "--sub-tx-file"
        , unsignedSubTxFile
        , "--witness-file"
        , witnessFile
        , "--out-file"
        , signedSubTxFile
        ]
    H.diffFileVsGoldenFile signedSubTxFile (goldenDir <> "/signed_out")

    -- The id is the body hash, so signing must not change it
    unsignedId <-
      execCardanoCLI
        [ "dijkstra"
        , "sub-transaction"
        , "txid"
        , "--sub-tx-file"
        , unsignedSubTxFile
        , "--output-text"
        ]
    signedId <-
      execCardanoCLI
        [ "dijkstra"
        , "sub-transaction"
        , "txid"
        , "--signed-sub-tx-file"
        , signedSubTxFile
        , "--output-text"
        ]
    unsignedId === signedId
    H.diffVsGoldenFile signedId (goldenDir <> "/txid_out")

    -- Embed it in a top-level transaction
    txBodyFile <- noteTempFile tempDir "tx.body"
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "build-raw"
        , "--tx-in"
        , "63e6a9a8e58e48cc025cae04daaed9d36fc7b70bc292721d9f5057ae37b24981#0"
        , "--tx-out"
        , "addr_test1vp0t4dfa9ktc2uvv7sg9leafuhtwyu0xcj4q4kf5pqkpjwqhklklg+15000002800000"
        , "--fee"
        , "200000"
        , "--signed-sub-tx-file"
        , signedSubTxFile
        , "--out-file"
        , txBodyFile
        ]
    H.diffFileVsGoldenFile txBodyFile (goldenDir <> "/tx_body_with_sub_tx_out")

    -- Check the final signed transaction and its identifier too.
    signedTxFile <- noteTempFile tempDir "tx.signed"
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "sign"
        , "--tx-body-file"
        , txBodyFile
        , "--signing-key-file"
        , signingKeyFile
        , "--testnet-magic"
        , "42"
        , "--out-file"
        , signedTxFile
        ]
    H.diffFileVsGoldenFile signedTxFile (goldenDir <> "/tx_signed_with_sub_tx_out")
    txId <-
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "txid"
        , "--tx-file"
        , signedTxFile
        , "--output-text"
        ]
    H.diffVsGoldenFile txId (goldenDir <> "/tx_signed_with_sub_tx_id_out")
