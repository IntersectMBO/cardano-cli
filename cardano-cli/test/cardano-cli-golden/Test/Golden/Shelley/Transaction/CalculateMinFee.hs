{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Transaction.CalculateMinFee
where

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.File qualified as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelleyTransactionCalculateMinFee :: Property
hprop_golden_shelleyTransactionCalculateMinFee = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  protocolParamsJsonFile <-
    noteInputFile
      "test/cardano-cli-golden/files/golden/shelley/transaction-calculate-min-fee/protocol-params.json"
  txBodyFile <- noteInputFile "test/cardano-cli-golden/files/golden/shelley/tx/txbody"
  minFeeTxtFile <- noteTempFile tempDir "min-fee.txt"

  minFeeTxt <-
    execCardanoCLI
      [ "transaction"
      , "calculate-min-fee"
      , "--tx-in-count"
      , "32"
      , "--tx-out-count"
      , "27"
      , "--byron-witness-count"
      , "5"
      , "--witness-count"
      , "10"
      , "--testnet-magic"
      , "4036000900"
      , "--protocol-params-file"
      , protocolParamsJsonFile
      , "--tx-body-file"
      , txBodyFile
      ]

  H.writeFile minFeeTxtFile minFeeTxt

  H.assertFileOccurences 1 "5083100" minFeeTxtFile
  H.assertFileLines (== 1) minFeeTxtFile
  H.assertEndsWithSingleNewline minFeeTxtFile
