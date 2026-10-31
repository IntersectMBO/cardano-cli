{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Debug.TransactionView where

import Control.Monad (forM, void, when)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import System.FilePath ((</>))

import Test.Cardano.CLI.Util (execCardanoCLI, watchdogProp)

import Hedgehog (Property, (===))
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

-- ----------------------------------------------------------------------------
-- Dijkstra era tests
-- ----------------------------------------------------------------------------

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
              ["dijkstra", "sub-transaction", "sign"]
                ++ ["--sub-tx-file", body, "--signing-key-file", key, "--out-file", out]
          pure out
    firstSub <-
      buildSub "1" $
        ["--account-balance-interval", account ++ ">=100,<200"]
          ++ ["--account-balance-interval", otherAccount ++ "=42"]
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
          ["dijkstra", "transaction", "sign"]
            ++ ["--tx-body-file", bodyFile, "--signing-key-file", key, "--out-file", signedFile]
    let (fileFlag, file, suffix) =
          if signed then ("--tx-file", signedFile, "-signed") else ("--tx-body-file", bodyFile, "")
    result <- execCardanoCLI ["debug", "transaction", "view", fileFlag, file, "--output-" ++ format]
    H.diffVsGoldenFile result $
      goldenDir </> "dijkstra/transaction-view-intervals" ++ suffix ++ "." ++ format

-- | A sub-transaction with two key witnesses and a mint under a native script policy.
hprop_golden_debug_transaction_view_dijkstra_sub_tx_witnesses_mint_json :: Property
hprop_golden_debug_transaction_view_dijkstra_sub_tx_witnesses_mint_json =
  goldenDijkstraAppliedTx "sub-tx-witnesses-mint" "json"

hprop_golden_debug_transaction_view_dijkstra_sub_tx_witnesses_mint_yaml :: Property
hprop_golden_debug_transaction_view_dijkstra_sub_tx_witnesses_mint_yaml =
  goldenDijkstraAppliedTx "sub-tx-witnesses-mint" "yaml"

-- | A sub-transaction with metadata and two native auxiliary scripts.
hprop_golden_debug_transaction_view_dijkstra_sub_tx_metadata_aux_scripts_json :: Property
hprop_golden_debug_transaction_view_dijkstra_sub_tx_metadata_aux_scripts_json =
  goldenDijkstraAppliedTx "sub-tx-metadata-aux-scripts" "json"

hprop_golden_debug_transaction_view_dijkstra_sub_tx_metadata_aux_scripts_yaml :: Property
hprop_golden_debug_transaction_view_dijkstra_sub_tx_metadata_aux_scripts_yaml =
  goldenDijkstraAppliedTx "sub-tx-metadata-aux-scripts" "yaml"

-- | Three sub-transactions given in an order that is sorted neither up nor down
-- by id. The view must keep the order given.
hprop_golden_debug_transaction_view_dijkstra_sub_tx_order_json :: Property
hprop_golden_debug_transaction_view_dijkstra_sub_tx_order_json =
  goldenDijkstraAppliedTx "sub-tx-order" "json"

hprop_golden_debug_transaction_view_dijkstra_sub_tx_order_yaml :: Property
hprop_golden_debug_transaction_view_dijkstra_sub_tx_order_yaml =
  goldenDijkstraAppliedTx "sub-tx-order" "yaml"

-- | A sub-transaction with the body fields that the other sub-transaction tests
-- leave empty: reference inputs, a validity range, a guard, a certificate, a
-- withdrawal, a direct deposit, a proposal, a vote, the current treasury value and
-- a treasury donation.
hprop_golden_debug_transaction_view_dijkstra_sub_tx_body_fields_json :: Property
hprop_golden_debug_transaction_view_dijkstra_sub_tx_body_fields_json =
  goldenDijkstraAppliedTx "sub-tx-body-fields" "json"

hprop_golden_debug_transaction_view_dijkstra_sub_tx_body_fields_yaml :: Property
hprop_golden_debug_transaction_view_dijkstra_sub_tx_body_fields_yaml =
  goldenDijkstraAppliedTx "sub-tx-body-fields" "yaml"

-- | View a signed transaction that was applied on a local testnet. The
-- @signed.tx.origin@ file next to it records how it was built.
goldenDijkstraAppliedTx :: FilePath -> String -> Property
goldenDijkstraAppliedTx name format =
  watchdogProp . propertyOnce $ do
    result <-
      execCardanoCLI
        [ "debug"
        , "transaction"
        , "view"
        , "--tx-file"
        , inputDir </> "dijkstra/tx" </> name </> "signed.tx"
        , "--output-" ++ format
        ]
    H.diffVsGoldenFile result $ goldenDir </> "dijkstra/transaction-view-" ++ name ++ "." ++ format

-- | The id printed for each sub-transaction is the one @sub-transaction txid@
-- gives for its file. The files are listed in the order they were given.
hprop_golden_debug_transaction_view_dijkstra_sub_tx_ids :: Property
hprop_golden_debug_transaction_view_dijkstra_sub_tx_ids =
  watchdogProp . propertyOnce $ do
    let dir = inputDir </> "dijkstra/tx/sub-tx-order"
    result <-
      execCardanoCLI
        ["debug", "transaction", "view", "--tx-file", dir </> "signed.tx", "--output-json"]
    printedIds <-
      H.leftFail $
        Aeson.parseEither subTransactionIds
          =<< Aeson.eitherDecode (TL.encodeUtf8 (TL.pack result))
    txIds <-
      forM ["sub-tx-1.signed", "sub-tx-2.signed", "sub-tx-3.signed"] $ \file ->
        execCardanoCLI
          [ "dijkstra"
          , "sub-transaction"
          , "txid"
          , "--signed-sub-tx-file"
          , dir </> file
          , "--output-text"
          ]
    printedIds === map (Text.strip . Text.pack) txIds
 where
  subTransactionIds :: Aeson.Value -> Aeson.Parser [Text]
  subTransactionIds =
    Aeson.withObject "transaction" $ \tx ->
      tx .: "sub-transactions" >>= mapM (Aeson.withObject "sub-transaction" (.: "id"))
