{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.TextView.DecodeCbor where

import Test.Cardano.CLI.Hedgehog qualified as H
import Test.Cardano.CLI.Util

import Hedgehog (Property)

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelleyTextViewDecodeCbor :: Property
hprop_golden_shelleyTextViewDecodeCbor =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    unsignedTxFile <- noteInputFile "test/cardano-cli-golden/files/input/shelley/tx/unsigned.tx"
    decodedTxtFile <- noteTempFile tempDir "decoded.txt"

    -- Defaults to signing a Mainnet transaction.

    decodedTxt <-
      execCardanoCLI
        [ "latest"
        , "text-view"
        , "decode-cbor"
        , "--output-text"
        , "--file"
        , unsignedTxFile
        ]

    H.writeFile decodedTxtFile decodedTxt

    H.assertFileOccurences 1 "# int(4999998000)" decodedTxtFile
    H.assertFileOccurences 1 "# int(2000)" decodedTxtFile
    H.assertFileOccurences 1 "# int(1000)" decodedTxtFile

    H.assertEndsWithSingleNewline decodedTxtFile
    H.assertFileLines (>= 10) decodedTxtFile
