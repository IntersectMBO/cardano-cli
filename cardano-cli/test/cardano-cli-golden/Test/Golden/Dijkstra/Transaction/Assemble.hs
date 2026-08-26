{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Dijkstra.Transaction.Assemble where

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog.Extras.Test qualified as H

-- Check that a witness written by `transaction witness` in the Dijkstra era
-- can be read back by `transaction assemble` to form a transaction.
-- Regression test for https://github.com/IntersectMBO/cardano-cli/issues/1422

hprop_golden_dijkstra_transaction_assemble_witness_signing_key :: Property
hprop_golden_dijkstra_transaction_assemble_witness_signing_key =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    txBodyFile <- noteTempFile tempDir "tx-body"

    -- Create tx body file
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "build-raw"
        , "--tx-in"
        , "2392d2b1200b5139fe555c81261697b29a8ccf561c5c783d46e78a479d977053#0"
        , "--tx-out"
        , "addr1q94cxl99qvtwunsqqv6g9mgj3zrawtpt4edsgwxkjtwpy5dsezcht90tmwfur7t5hc9fk8hjd3r5vjwec2h8vmk3xh8s7er7t3+100"
        , "--fee"
        , "12"
        , "--tx-body-file"
        , txBodyFile
        ]

    -- Sign it with a single signing key, as a detached witness file
    witnessFile <- noteTempFile tempDir "single-signing-key-witness"
    signingKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/conway/keys/payment_keys/signing_key"

    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "witness"
        , "--tx-body-file"
        , txBodyFile
        , "--signing-key-file"
        , signingKeyFile
        , "--mainnet"
        , "--out-file"
        , witnessFile
        ]

    H.assertFileOccurences 1 "TxWitness DijkstraEra" witnessFile

    -- Assemble the body and the witness back into a signed transaction
    signedTxFile <- noteTempFile tempDir "signed-tx"
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "assemble"
        , "--tx-body-file"
        , txBodyFile
        , "--witness-file"
        , witnessFile
        , "--out-file"
        , signedTxFile
        ]

    H.assertFileOccurences 1 "Tx DijkstraEra" signedTxFile
