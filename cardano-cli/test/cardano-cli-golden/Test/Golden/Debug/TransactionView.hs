{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Debug.TransactionView where

import Control.Monad (void)
import System.FilePath ((</>))

import Test.Cardano.CLI.Util (execCardanoCLI, watchdogProp)

import Hedgehog (Property)
import Hedgehog.Extras.Test (propertyOnce)
import Hedgehog.Extras.Test qualified as H

inputDir, goldenDir :: FilePath
inputDir = "test/cardano-cli-golden/files/input"
goldenDir = "test/cardano-cli-golden/files/golden"

-- ----------------------------------------------------------------------------
-- Conway era tests
-- ----------------------------------------------------------------------------

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden debug transaction view conway json/"'@
hprop_golden_debug_transaction_view_conway_json :: Property
hprop_golden_debug_transaction_view_conway_json =
  watchdogProp . propertyOnce $ do
    result <-
      execCardanoCLI
        [ "debug"
        , "transaction"
        , "view"
        , "--tx-file"
        , inputDir </> "conway/tx/signed.tx"
        , "--output-json"
        ]
    H.diffVsGoldenFile result $ goldenDir </> "conway/transaction-view.json"

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden debug transaction view conway yaml/"'@
hprop_golden_debug_transaction_view_conway_yaml :: Property
hprop_golden_debug_transaction_view_conway_yaml =
  watchdogProp . propertyOnce $ do
    result <-
      execCardanoCLI
        [ "debug"
        , "transaction"
        , "view"
        , "--tx-file"
        , inputDir </> "conway/tx/signed.tx"
        , "--output-yaml"
        ]
    H.diffVsGoldenFile result $ goldenDir </> "conway/transaction-view.yaml"

-- | Deposits must be visible when inspecting a transaction before signing.
hprop_golden_debug_transaction_view_dijkstra_direct_deposits_json :: Property
hprop_golden_debug_transaction_view_dijkstra_direct_deposits_json =
  goldenDijkstraDirectDeposits "json"

hprop_golden_debug_transaction_view_dijkstra_direct_deposits_yaml :: Property
hprop_golden_debug_transaction_view_dijkstra_direct_deposits_yaml =
  goldenDijkstraDirectDeposits "yaml"

goldenDijkstraDirectDeposits :: String -> Property
goldenDijkstraDirectDeposits format =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    bodyFile <- H.noteTempFile tempDir "tx.body"
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "build-raw"
        , "--tx-in"
        , "63e6a9a8e58e48cc025cae04daaed9d36fc7b70bc292721d9f5057ae37b24981#0"
        , "--tx-out"
        , "addr_test1vp0t4dfa9ktc2uvv7sg9leafuhtwyu0xcj4q4kf5pqkpjwqhklklg+5000000"
        , "--fee"
        , "200000"
        , "--direct-deposit"
        , "stake_test17qvxuvh64q9zdqgrjt76d42eclk5wgdxtnsun4808cwg0dqxv5r99+20000000"
        , "--direct-deposit"
        , "stake_test1uz855drx5szvz845zqcnq9dc3ez8mqdkqzy7yhmpzcqwvpg59zzfl+30000000"
        , "--out-file"
        , bodyFile
        ]
    result <-
      execCardanoCLI
        [ "debug"
        , "transaction"
        , "view"
        , "--tx-body-file"
        , bodyFile
        , "--output-" ++ format
        ]
    H.diffVsGoldenFile result $ goldenDir </> "dijkstra/transaction-view-direct-deposits." ++ format
