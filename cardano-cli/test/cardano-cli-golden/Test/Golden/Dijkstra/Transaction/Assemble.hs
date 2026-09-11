{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Dijkstra.Transaction.Assemble where

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog.Extras.Test qualified as H

-- Check that a witness written by `transaction witness` in the Dijkstra era
-- can be read back by `transaction assemble` to form a transaction.
-- Regression test for https://github.com/IntersectMBO/cardano-cli/issues/1422
--
-- The exact transaction pinned by the golden files below was submitted to and
-- accepted by a local Dijkstra testnet (magic 42).

hprop_golden_dijkstra_transaction_assemble_witness_signing_key :: Property
hprop_golden_dijkstra_transaction_assemble_witness_signing_key =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    -- Use a pre-built tx body golden file (build-raw requires EraCommonConstraints
    -- which are not yet implemented for DijkstraEra in cardano-api)
    txBodyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/dijkstra/transaction/tx_body"

    -- Sign it with a single signing key, as a detached witness file
    witnessFile <- noteTempFile tempDir "single-signing-key-witness"
    signingKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/dijkstra/keys/utxo_keys/signing_key"

    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "witness"
        , "--tx-body-file"
        , txBodyFile
        , "--signing-key-file"
        , signingKeyFile
        , "--testnet-magic"
        , "42"
        , "--out-file"
        , witnessFile
        ]

    goldenWitnessFile <- H.note "test/cardano-cli-golden/files/golden/dijkstra/transaction/witness_out"
    H.diffFileVsGoldenFile witnessFile goldenWitnessFile

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

    goldenSignedTxFile <-
      H.note "test/cardano-cli-golden/files/golden/dijkstra/transaction/assemble_out"
    H.diffFileVsGoldenFile signedTxFile goldenSignedTxFile
