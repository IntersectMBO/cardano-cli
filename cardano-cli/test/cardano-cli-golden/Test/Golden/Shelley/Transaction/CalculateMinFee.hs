{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Transaction.CalculateMinFee
   where

import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelley_transaction_calculate_min_fee :: Property
hprop_golden_shelley_transaction_calculate_min_fee = propertyOnce $ do
  protocolParamsJsonFile <- noteInputFile "test/cardano-cli-golden/files/input/shelley/transaction-calculate-min-fee/protocol-params.json"
  txBodyFile <- noteInputFile "test/cardano-cli-golden/files/input/shelley/tx/txbody"

  minFeeTxt <- execCardanoCLI
    [ "transaction","calculate-min-fee"
    , "--byron-witness-count", "10"
    , "--witness-count", "5"
    , "--protocol-params-file", protocolParamsJsonFile
    , "--reference-script-size", "0"
    , "--tx-body-file", txBodyFile
    ]

  H.diff minFeeTxt (==) "2050100 Lovelace\n"
