{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.TextView.DecodeCbor where

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelleyTextViewDecodeCbor :: Property
hprop_golden_shelleyTextViewDecodeCbor = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  unsignedTxFile <- noteInputFile "test/cardano-cli-golden/files/golden/shelley/tx/unsigned.tx"
  decodedTxtFile <- noteTempFile tempDir "decoded.txt"

  -- Defaults to signing a Mainnet transaction.

  decodedTxt <-
    execCardanoCLI
      [ "text-view"
      , "decode-cbor"
      , "--file"
      , unsignedTxFile
      ]

  H.writeFile decodedTxtFile decodedTxt

  H.assertFileOccurences 1 "# int(4999998000)" decodedTxtFile
  H.assertFileOccurences 1 "# int(2000)" decodedTxtFile
  H.assertFileOccurences 1 "# int(1000)" decodedTxtFile

  H.assertEndsWithSingleNewline decodedTxtFile
  H.assertFileLines (>= 10) decodedTxtFile
