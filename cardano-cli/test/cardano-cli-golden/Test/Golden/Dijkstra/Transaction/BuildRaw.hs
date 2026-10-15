{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Golden.Dijkstra.Transaction.BuildRaw where

import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Control.Monad (void)
import Data.Aeson qualified as Aeson

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog qualified as H
import Hedgehog.Extras.Test qualified as H

-- A Plutus V4 script file must work as a direct witness for every script purpose.
-- Regression test for https://github.com/IntersectMBO/cardano-cli/issues/1448

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden dijkstra build raw plutus v4 script witnesses/"'@
hprop_golden_dijkstra_build_raw_plutus_v4_script_witnesses :: Property
hprop_golden_dijkstra_build_raw_plutus_v4_script_witnesses =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    pparamsFile <- noteTempFile tempDir "protocol-params.json"
    proposalFile <- noteTempFile tempDir "info.action"
    txBodyFile <- noteTempFile tempDir "tx-body"

    let v4Script = "test/cardano-cli-golden/files/input/dijkstra/plutus/v4-always-succeeds.plutus"
        v4ScriptHash = "31a78786b5989dc6fd2d4ab15b297069e94106ecacceb8eb29e1b681"

    -- build-raw does not evaluate scripts, so empty protocol parameters suffice.
    H.evalIO . Aeson.encodeFile pparamsFile . L.PParams $
      L.emptyPParamsIdentity @(Exp.LedgerEra Exp.DijkstraEra)

    void $
      execCardanoCLI
        [ "dijkstra"
        , "governance"
        , "action"
        , "create-info"
        , "--testnet"
        , "--governance-action-deposit"
        , "12345"
        , "--deposit-return-stake-verification-key-file"
        , "test/cardano-cli-golden/files/input/governance/stake-address.vkey"
        , "--anchor-url"
        , "https://example.com"
        , "--anchor-data-hash"
        , "52e69500a92d80f2126c836a4903dc582006709f004cf7a28ed648f732dff8d2"
        , "--out-file"
        , proposalFile
        ]

    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "build-raw"
        , "--tx-in"
        , "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891#200"
        , "--tx-in-script-file"
        , v4Script
        , "--tx-in-datum-value"
        , "24"
        , "--tx-in-redeemer-value"
        , "42"
        , "--tx-in-execution-units"
        , "(100,110)"
        , "--mint"
        , "1000 " <> v4ScriptHash
        , "--mint-script-file"
        , v4Script
        , "--mint-redeemer-value"
        , "42"
        , "--mint-execution-units"
        , "(100,110)"
        , "--withdrawal"
        , "stake_test17qvxuvh64q9zdqgrjt76d42eclk5wgdxtnsun4808cwg0dqxv5r99+10000"
        , "--withdrawal-script-file"
        , v4Script
        , "--withdrawal-redeemer-value"
        , "83"
        , "--withdrawal-execution-units"
        , "(100,110)"
        , "--certificate-file"
        , "test/cardano-cli-golden/files/input/stake-address-registration.json"
        , "--certificate-script-file"
        , v4Script
        , "--certificate-redeemer-value"
        , "63"
        , "--certificate-execution-units"
        , "(100,110)"
        , "--vote-file"
        , "test/cardano-cli-golden/files/input/conway/vote1.drep.json"
        , "--vote-script-file"
        , v4Script
        , "--vote-redeemer-value"
        , "1"
        , "--vote-execution-units"
        , "(100,110)"
        , "--proposal-file"
        , proposalFile
        , "--proposal-script-file"
        , v4Script
        , "--proposal-redeemer-value"
        , "1"
        , "--proposal-execution-units"
        , "(100,110)"
        , "--tx-out"
        , "addr_test1vp0t4dfa9ktc2uvv7sg9leafuhtwyu0xcj4q4kf5pqkpjwqhklklg+15000000"
        , "--fee"
        , "200000"
        , "--protocol-params-file"
        , pparamsFile
        , "--out-file"
        , txBodyFile
        ]

    result <-
      execCardanoCLI
        ["debug", "transaction", "view", "--tx-body-file", txBodyFile, "--output-yaml"]
    H.diffVsGoldenFile
      result
      "test/cardano-cli-golden/files/golden/dijkstra/transaction/build-raw-plutus-v4-script-witnesses.out"
