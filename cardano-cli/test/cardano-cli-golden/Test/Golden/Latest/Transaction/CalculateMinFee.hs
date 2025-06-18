{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Golden.Latest.Transaction.CalculateMinFee
  ( hprop_golden_latest_transaction_calculate_min_fee
  , hprop_golden_latest_transaction_calculate_min_fee_flags
  )
where

import Control.Monad
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import System.FilePath ((</>))

import Test.Cardano.CLI.Util

import Hedgehog (Property, (===))
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_latest_transaction_calculate_min_fee :: Property
hprop_golden_latest_transaction_calculate_min_fee =
  watchdogProp . propertyOnce $ do
    protocolParamsJsonFile <-
      noteInputFile
        "test/cardano-cli-golden/files/input/transaction/calculate-min-fee/protocol-params-preview.json"
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

    Aeson.decode (TL.encodeUtf8 (TL.pack minFeeTxt)) === Just (Aeson.object ["fee" .= (165897 :: Int)])

{- HLINT ignore "Use camelCase" -}

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden shelley transaction calculate min fee/"'@
hprop_golden_latest_transaction_calculate_min_fee_flags :: Property
hprop_golden_latest_transaction_calculate_min_fee_flags = do
  let supplyValues =
        [ []
        , ["--output-json"]
        , ["--output-text"]
        , ["--output-json", "--out-file"]
        , ["--output-text", "--out-file"]
        ]
  watchdogProp . propertyOnce $ forM_ supplyValues $ \flags ->
    H.moduleWorkspace "tmp" $ \tempDir -> do
      protocolParamsJsonFile <-
        noteInputFile
          "test/cardano-cli-golden/files/input/transaction/calculate-min-fee/flags-protocol-params-preview.json"
      txBodyFile <- noteInputFile "test/cardano-cli-golden/files/input/conway/tx/txbody"
      let outFileFp = tempDir </> "out.txt"
          outFile =
            case flags of
              _ : ["--out-file"] -> [outFileFp]
              _ -> []

      minFeeTxt <-
        execCardanoCLI $
          [ "latest"
          , "transaction"
          , "calculate-min-fee"
          , "--byron-witness-count"
          , "10"
          , "--witness-count"
          , "5"
          , "--protocol-params-file"
          , protocolParamsJsonFile
          , "--reference-script-size"
          , "0"
          , "--tx-body-file"
          , txBodyFile
          ]
            ++ flags
            ++ outFile

      case flags of
        [] ->
          Aeson.decode (TL.encodeUtf8 (TL.pack minFeeTxt)) === Just (Aeson.object ["fee" .= (247473 :: Int)])
        ["--output-text"] ->
          H.diff minFeeTxt (==) "247473 Lovelace\n"
        ["--output-text", "--out-file"] -> do
          textOnDisk <- H.readFile outFileFp
          H.diff textOnDisk (==) "247473 Lovelace"
        ["--output-json"] -> do
          let jsonFromStdout = Aeson.decode $ TL.encodeUtf8 $ TL.pack minFeeTxt
          H.diff jsonFromStdout (==) (Just $ Aeson.object ["fee" .= (247473 :: Int)])
        ["--output-json", "--out-file"] -> do
          jsonOnDisk :: Aeson.Value <- H.readJsonFileOk outFileFp
          H.diff jsonOnDisk (==) (Aeson.object ["fee" .= (247473 :: Int)])
        _ -> do
          H.note_ ("Unexpected flags:" <> show flags)
          H.assert False
