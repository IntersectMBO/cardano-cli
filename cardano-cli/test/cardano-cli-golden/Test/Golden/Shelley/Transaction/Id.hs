{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Transaction.Id where

import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Golden as H

{- HLINT ignore "Use camelCase" -}

-- Execute this test with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden shelley transaction id/"'@
hprop_golden_shelley_transaction_id :: Property
hprop_golden_shelley_transaction_id = propertyOnce $ do
  txFile <- noteInputFile "test/cardano-cli-golden/files/input/shelley/tx-for-txid.json"

  let baseCmd = ["latest", "transaction", "txid", "--tx-file", txFile]

  output1 <- execCardanoCLI baseCmd
  goldenFile1 <- H.note "test/cardano-cli-golden/files/golden/shelley/transaction-id-flagless"
  H.diffVsGoldenFile output1 goldenFile1
