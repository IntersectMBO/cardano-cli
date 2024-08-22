{-# LANGUAGE TypeApplications #-}

module Test.Cli.Shelley.Transaction.BuildEstimate where

import           Control.Lens ((^?))
import           Data.Aeson as A
import qualified Data.Aeson.Lens as A
import           GHC.Exts (IsString (..))
import           System.FilePath ((</>))

import           Test.Cardano.CLI.Util

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

inputDir :: FilePath
inputDir = "test/cardano-cli-test/files/input/shelley/transaction"

txIn :: String
txIn = "e7b75bc29c8de752774771810a52fbd55444940c1b8bd04359aeb0f026c866ba#1"

policyId :: IsString s => s
policyId = "e2b715a86bee4f14fef84081217f9e2646893a7d60a38af69e0aa572"

asset :: IsString s => s
asset = "4964656e74697479204e4654"

coldLockScriptAddr :: IsString s => s
coldLockScriptAddr = "addr_test1wr3tw9dgd0hy7987lpqgzgtlncnydzf604s28zhknc922uskgmccs"

changeAddress :: String
changeAddress = "addr_test1vrz966ll936u04ge6ehg8vv3uc3nm8gg0we9xkm86m8k7pcryd6en"

hprop_conway_transaction_build_estimate_mint_balance_manual :: Property
hprop_conway_transaction_build_estimate_mint_balance_manual = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  txFile <- H.noteTempFile tempDir "tx.traw"
  _ <-
    execCardanoCLI
      [ "conway"
      , "transaction"
      , "build-estimate"
      , "--tx-in"
      , txIn
      , "--tx-in-collateral"
      , txIn
      , "--mint"
      , "1 " <> policyId <> "." <> asset
      , "--tx-out"
      , -- here we manually add 1 nft to the output
        coldLockScriptAddr <> "+5000000+1 " <> policyId <> "." <> asset
      , "--tx-out-inline-datum-file"
      , inputDir </> "initColdLockScriptDatum.json"
      , "--change-address"
      , changeAddress
      , "--mint-script-file"
      , inputDir </> "coldAlwaysTrueMint.plutus"
      , "--mint-redeemer-value"
      , "{}"
      , "--out-file"
      , txFile
      , "--shelley-key-witnesses"
      , "1"
      , "--total-utxo-value"
      , "100000000"
      , "--mint-execution-units"
      , "(1,1)"
      , "--protocol-params-file"
      , inputDir </> "buildEstimateProtocolParams.json"
      ]

  stdout <-
    H.noteM $
      execCardanoCLI
        ["debug", "transaction", "view", "--tx-file", txFile]

  res <- H.leftFail $ A.eitherDecode @A.Value (fromString stdout)
  outputs <- H.nothingFail $ res ^? A.key "outputs" . A._Array
  let outputsWithMintedAsset =
        foldl
          ( \acc o ->
              if o ^? A.key "address" . A._String == Just coldLockScriptAddr
                && o
                  ^? A.key "amount"
                    . A.key ("policy " <> policyId)
                    . A.key ("asset " <> asset)
                    . A._Number
                  == Just 1
                then o : acc
                else acc
          )
          []
          outputs
  H.note_ $ "Check for NFT at output " <> coldLockScriptAddr
  length outputsWithMintedAsset === 1

hprop_conway_transaction_build_estimate_mint_balance_auto :: Property
hprop_conway_transaction_build_estimate_mint_balance_auto = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  txFile <- H.noteTempFile tempDir "tx.traw"
  _ <-
    execCardanoCLI
      [ "conway"
      , "transaction"
      , "build-estimate"
      , "--tx-in"
      , txIn
      , "--tx-in-collateral"
      , txIn
      , "--mint"
      , "1 " <> policyId <> "." <> asset
      , "--tx-out"
      , -- there is no NFT in this output - autobalance this
        coldLockScriptAddr <> "+5000000"
      , "--tx-out-inline-datum-file"
      , inputDir </> "initColdLockScriptDatum.json"
      , "--change-address"
      , changeAddress
      , "--mint-script-file"
      , inputDir </> "coldAlwaysTrueMint.plutus"
      , "--mint-redeemer-value"
      , "{}"
      , "--out-file"
      , txFile
      , "--shelley-key-witnesses"
      , "1"
      , "--total-utxo-value"
      , "100000000"
      , "--mint-execution-units"
      , "(1,1)"
      , "--protocol-params-file"
      , inputDir </> "buildEstimateProtocolParams.json"
      ]

  stdout <-
    H.noteM $
      execCardanoCLI
        ["debug", "transaction", "view", "--tx-file", txFile]

  res <- H.leftFail $ A.eitherDecode @A.Value (fromString stdout)
  outputs <- H.nothingFail $ res ^? A.key "outputs" . A._Array
  let outputsWithMintedAsset =
        foldl
          ( \acc o ->
              if o ^? A.key "address" . A._String == Just coldLockScriptAddr
                && o
                  ^? A.key "amount"
                    . A.key ("policy " <> policyId)
                    . A.key ("asset " <> asset)
                    . A._Number
                  == Just 1
                then o : acc
                else acc
          )
          []
          outputs
  H.note_ $ "Check for NFT at output " <> coldLockScriptAddr
  length outputsWithMintedAsset === 1
  H.failure
