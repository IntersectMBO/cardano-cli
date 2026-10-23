{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Debug.TransactionView where

import Control.Monad (void, when)
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

hprop_golden_debug_transaction_view_dijkstra_intervals_json :: Property
hprop_golden_debug_transaction_view_dijkstra_intervals_json = goldenDijkstraIntervals "json" False

hprop_golden_debug_transaction_view_dijkstra_intervals_yaml :: Property
hprop_golden_debug_transaction_view_dijkstra_intervals_yaml = goldenDijkstraIntervals "yaml" False

hprop_golden_debug_transaction_view_dijkstra_signed_intervals_json :: Property
hprop_golden_debug_transaction_view_dijkstra_signed_intervals_json = goldenDijkstraIntervals "json" True

hprop_golden_debug_transaction_view_dijkstra_signed_intervals_yaml :: Property
hprop_golden_debug_transaction_view_dijkstra_signed_intervals_yaml = goldenDijkstraIntervals "yaml" True

-- Include every interval constructor, the same account at different levels,
-- and a second sub-transaction without intervals to exercise ordering and nulls.
goldenDijkstraIntervals :: String -> Bool -> Property
goldenDijkstraIntervals format signed =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    bodyFile <- H.noteTempFile tempDir "tx.body"
    signedFile <- H.noteTempFile tempDir "tx.signed"
    let account = "stake_test17qvxuvh64q9zdqgrjt76d42eclk5wgdxtnsun4808cwg0dqxv5r99"
        otherAccount = "stake_test1uz855drx5szvz845zqcnq9dc3ez8mqdkqzy7yhmpzcqwvpg59zzfl"
        input ix = "63e6a9a8e58e48cc025cae04daaed9d36fc7b70bc292721d9f5057ae37b24981#" ++ ix
        output = "addr_test1vp0t4dfa9ktc2uvv7sg9leafuhtwyu0xcj4q4kf5pqkpjwqhklklg+5000000"
        key = inputDir </> "dijkstra/keys/utxo_keys/signing_key"
        buildSub ix args = do
          body <- H.noteTempFile tempDir $ "sub-" ++ ix ++ ".body"
          out <- H.noteTempFile tempDir $ "sub-" ++ ix ++ ".signed"
          void $
            execCardanoCLI $
              concat
                [ ["dijkstra", "sub-transaction", "build-raw"]
                , ["--tx-in", input ix, "--tx-out", output, "--out-file", body]
                , args
                ]
          void $
            execCardanoCLI $
              concat
                [ ["dijkstra", "sub-transaction", "sign"]
                , ["--sub-tx-file", body, "--signing-key-file", key, "--out-file", out]
                ]
          pure out
    firstSub <-
      buildSub "1" $
        concat
          [ ["--account-balance-interval", account ++ ">=100,<200"]
          , ["--account-balance-interval", otherAccount ++ "=42"]
          ]
    secondSub <- buildSub "2" []
    void $
      execCardanoCLI $
        concat
          [ ["dijkstra", "transaction", "build-raw"]
          , ["--tx-in", input "0", "--tx-out", output, "--fee", "200000"]
          , ["--account-balance-interval", account ++ ">=100"]
          , ["--account-balance-interval", otherAccount ++ "<200"]
          , ["--starting-account-balance-interval", account ++ "=300"]
          , ["--starting-account-balance-interval", otherAccount ++ ">=0,<400"]
          , ["--signed-sub-tx-file", firstSub, "--signed-sub-tx-file", secondSub]
          , ["--out-file", bodyFile]
          ]
    when signed $
      void $
        execCardanoCLI $
          concat
            [ ["dijkstra", "transaction", "sign"]
            , ["--tx-body-file", bodyFile, "--signing-key-file", key, "--out-file", signedFile]
            ]
    let (fileFlag, file, suffix) =
          if signed then ("--tx-file", signedFile, "-signed") else ("--tx-body-file", bodyFile, "")
    result <- execCardanoCLI ["debug", "transaction", "view", fileFlag, file, "--output-" ++ format]
    H.diffVsGoldenFile result $
      goldenDir </> "dijkstra/transaction-view-intervals" ++ suffix ++ "." ++ format
