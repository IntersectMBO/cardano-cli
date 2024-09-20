{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Golden.Shelley.Transaction.CalculateMinFee where

import           Control.Monad (forM_)
import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           System.FilePath ((</>))

import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

{- HLINT ignore "Use camelCase" -}

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden shelley transaction calculate min fee/"'@
hprop_golden_shelley_transaction_calculate_min_fee :: Property
hprop_golden_shelley_transaction_calculate_min_fee = do
  let supplyValues =
        [ []
        , ["--output-json"]
        , ["--output-text"]
        , ["--output-json", "--out-file"]
        , ["--output-text", "--out-file"]
        ]
  propertyOnce $ forM_ supplyValues $ \flags ->
    H.moduleWorkspace "tmp" $ \tempDir -> do
      protocolParamsJsonFile <-
        noteInputFile
          "test/cardano-cli-golden/files/input/shelley/transaction-calculate-min-fee/protocol-params.json"
      txBodyFile <- noteInputFile "test/cardano-cli-golden/files/input/shelley/tx/txbody"
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
          H.diff minFeeTxt (==) "2050100 Lovelace\n"
        ["--output-text"] ->
          H.diff minFeeTxt (==) "2050100 Lovelace\n"
        ["--output-text", "--out-file"] -> do
          textOnDisk <- H.readFile outFileFp
          H.diff textOnDisk (==) "2050100 Lovelace"
        ["--output-json"] -> do
          let jsonFromStdout = Aeson.decode $ TL.encodeUtf8 $ TL.pack minFeeTxt
          H.diff jsonFromStdout (==) (Just $ Aeson.object ["fee" .= (2050100 :: Int)])
        ["--output-json", "--out-file"] -> do
          jsonOnDisk :: Aeson.Value <- H.readJsonFileOk outFileFp
          H.diff jsonOnDisk (==) (Aeson.object ["fee" .= (2050100 :: Int)])
        _ -> do
          H.note_ ("Unexpected flags:" <> show flags)
          H.assert False
