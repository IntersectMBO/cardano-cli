{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Babbage.Transaction.CalculateMinFee
  ( hprop_golden_babbage_transaction_calculate_min_fee
  )
where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL

import Test.Cardano.CLI.Util

import Hedgehog (Property, (===))

{- HLINT ignore "Use camelCase" -}

hprop_golden_babbage_transaction_calculate_min_fee :: Property
hprop_golden_babbage_transaction_calculate_min_fee = propertyOnce $ do
  protocolParamsJsonFile <-
    noteInputFile
      "test/cardano-cli-golden/files/input/conway/transaction-calculate-min-fee/protocol-params.json"
  txBodyFile <- noteInputFile "test/cardano-cli-golden/files/input/conway/tx/txbody"

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

  Aeson.decode (TL.encodeUtf8 (TL.pack minFeeTxt)) === Just (Aeson.object ["fee" .= (165633 :: Int)])
