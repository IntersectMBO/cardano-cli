{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Babbage.Transaction.CalculateMinFee
  ( hprop_golden_babbage_transaction_calculate_min_fee
  )
where

import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_babbage_transaction_calculate_min_fee :: Property
hprop_golden_babbage_transaction_calculate_min_fee = propertyOnce $ do
  protocolParamsJsonFile <-
    noteInputFile
      "test/cardano-cli-golden/files/input/babbage/transaction-calculate-min-fee/protocol-params.json"
  txBodyFile <- noteInputFile "test/cardano-cli-golden/files/input/babbage/tx/txbody"

  minFeeTxt <-
    execCardanoCLI
      [ "latest"
      , "transaction"
      , "calculate-min-fee"
      , "--witness-count"
      , "1"
      , "--protocol-params-file"
      , protocolParamsJsonFile
      , "--reference-script-size"
      , "0"
      , "--tx-body-file"
      , txBodyFile
      ]

  H.diff minFeeTxt (==) "165633 Lovelace\n"
