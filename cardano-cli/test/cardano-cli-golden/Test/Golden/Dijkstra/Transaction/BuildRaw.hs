{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Dijkstra.Transaction.BuildRaw where

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog.Extras.Test qualified as H

-- Check that Plutus V4 reference script witnesses are accepted by
-- `dijkstra transaction build-raw` for every witness kind that supports
-- them, and that they appear correctly in `debug transaction view`.
-- Regression test for https://github.com/IntersectMBO/cardano-cli/issues/1448

hprop_golden_dijkstra_build_raw_plutus_v4_reference_witnesses :: Property
hprop_golden_dijkstra_build_raw_plutus_v4_reference_witnesses =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    txBodyFile <- noteTempFile tempDir "tx-body"

    -- Hash of test/cardano-cli-golden/files/input/dijkstra/plutus/v4-always-succeeds.plutus
    let v4ScriptHash = "31a78786b5989dc6fd2d4ab15b297069e94106ecacceb8eb29e1b681"

    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "build-raw"
        , -- Spending input witnessed by a Plutus V4 reference script
          "--tx-in"
        , "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891#200"
        , "--spending-tx-in-reference"
        , "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891#201"
        , "--spending-plutus-script-v4"
        , "--spending-reference-tx-in-inline-datum-present"
        , "--spending-reference-tx-in-redeemer-value"
        , "42"
        , "--spending-reference-tx-in-execution-units"
        , "(100,110)"
        , -- Certificate witnessed by a Plutus V4 reference script
          "--certificate-file"
        , "test/cardano-cli-golden/files/input/stake-address-registration.json"
        , "--certificate-tx-in-reference"
        , "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891#203"
        , "--certificate-plutus-script-v4"
        , "--certificate-reference-tx-in-redeemer-value"
        , "63"
        , "--certificate-reference-tx-in-execution-units"
        , "(100,110)"
        , -- Withdrawal witnessed by a Plutus V4 reference script
          "--withdrawal"
        , "stake_test17qvxuvh64q9zdqgrjt76d42eclk5wgdxtnsun4808cwg0dqxv5r99+10000"
        , "--withdrawal-tx-in-reference"
        , "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891#204"
        , "--withdrawal-plutus-script-v4"
        , "--withdrawal-reference-tx-in-redeemer-value"
        , "83"
        , "--withdrawal-reference-tx-in-execution-units"
        , "(100,110)"
        , -- Proposal witnessed by a Plutus V4 reference script
          "--proposal-file"
        , "test/cardano-cli-golden/files/input/conway/conway-create-protocol-parameters-update.action"
        , "--proposal-tx-in-reference"
        , "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891#205"
        , "--proposal-plutus-script-v4"
        , "--proposal-reference-tx-in-redeemer-value"
        , "1"
        , "--proposal-reference-tx-in-execution-units"
        , "(100,110)"
        , -- Vote witnessed by a Plutus V4 reference script
          "--vote-file"
        , "test/cardano-cli-golden/files/input/conway/vote1.drep.json"
        , "--vote-tx-in-reference"
        , "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891#206"
        , "--vote-plutus-script-v4"
        , "--vote-reference-tx-in-redeemer-value"
        , "1"
        , "--vote-reference-tx-in-execution-units"
        , "(100,110)"
        , -- Mint witnessed by a Plutus V4 reference script
          "--mint"
        , "1000 " <> v4ScriptHash
        , "--mint-tx-in-reference"
        , "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891#202"
        , "--mint-plutus-script-v4"
        , "--mint-reference-tx-in-redeemer-value"
        , "42"
        , "--mint-reference-tx-in-execution-units"
        , "(100,110)"
        , "--policy-id"
        , v4ScriptHash
        , -- Tx output and fee
          "--tx-out"
        , "addr_test1vp0t4dfa9ktc2uvv7sg9leafuhtwyu0xcj4q4kf5pqkpjwqhklklg+15000000"
        , "--fee"
        , "200000"
        , "--protocol-params-file"
        , "test/cardano-cli-golden/files/input/dijkstra/transaction/protocol-params-preview.json"
        , "--tx-body-file"
        , txBodyFile
        ]

    result <-
      execCardanoCLI
        ["debug", "transaction", "view", "--tx-body-file", txBodyFile, "--output-yaml"]
    H.diffVsGoldenFile
      result
      "test/cardano-cli-golden/files/golden/dijkstra/transaction/build-raw-plutus-v4-reference-witnesses.out"
