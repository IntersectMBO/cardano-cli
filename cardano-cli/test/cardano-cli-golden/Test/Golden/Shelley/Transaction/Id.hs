{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Transaction.Id where

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.Golden qualified as H

{- HLINT ignore "Use camelCase" -}

-- Execute this test with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden shelley transaction id/"'@
hprop_golden_shelley_transaction_id :: Property
hprop_golden_shelley_transaction_id = propertyOnce $ do
  txFile <- noteInputFile "test/cardano-cli-golden/files/input/shelley/tx-for-txid.json"

  let baseCmd = ["latest", "transaction", "txid", "--tx-file", txFile]

  output1 <- execCardanoCLI baseCmd
  goldenFile0 <- H.note "test/cardano-cli-golden/files/golden/shelley/transaction-id-default"
  H.diffVsGoldenFile output1 goldenFile0

  goldenFile1 <- H.note "test/cardano-cli-golden/files/golden/shelley/transaction-id-flagless"
  output2 <- execCardanoCLI $ baseCmd ++ ["--output-text"]
  H.diffVsGoldenFile output2 goldenFile1

  output3 <- execCardanoCLI $ baseCmd ++ ["--output-json"]
  goldenFile2 <- H.note "test/cardano-cli-golden/files/golden/shelley/transaction-id.json"
  H.diffVsGoldenFile output3 goldenFile2
