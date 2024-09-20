{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Transaction.Build where

import           Control.Monad (void)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC

import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Golden as H

{- HLINT ignore "Use camelCase" -}

txOut :: String
txOut =
  "addr1q94cxl99qvtwunsqqv6g9mgj3zrawtpt4edsgwxkjtwpy5dsezcht90tmwfur7t5hc9fk8hjd3r5vjwec2h8vmk3xh8s7er7t3+100"

txIn :: String
txIn = "2392d2b1200b5139fe555c81261697b29a8ccf561c5c783d46e78a479d977053#0"

hprop_golden_shelley_transaction_build :: Property
hprop_golden_shelley_transaction_build =
  propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    txBodyOutFile <- noteTempFile tempDir "tx-body-out"

    void $
      execCardanoCLI
        [ "mary"
        , "transaction"
        , "build-raw"
        , "--tx-in"
        , txIn
        , "--tx-out"
        , txOut
        , "--fee"
        , "12"
        , "--tx-body-file"
        , txBodyOutFile
        ]

    goldenFile <- H.note "test/cardano-cli-golden/files/golden/shelley/build-raw-tx-body-out-1.json"
    H.diffFileVsGoldenFile txBodyOutFile goldenFile

hprop_golden_shelley_transaction_build_certificate_script_witnessed :: Property
hprop_golden_shelley_transaction_build_certificate_script_witnessed =
  propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    let deregcert =
          "test/cardano-cli-golden/files/input/shelley/certificates/stake_address_deregistration_certificate"
        scriptWit = "test/cardano-cli-golden/files/input/shelley/multisig/scripts/any"

    txBodyOutFile <- noteTempFile tempDir "tx-body-out"

    void $
      execCardanoCLI
        [ "mary"
        , "transaction"
        , "build-raw"
        , "--tx-in"
        , txIn
        , "--tx-out"
        , txOut
        , "--certificate-file"
        , deregcert
        , "--certificate-script-file"
        , scriptWit
        , "--fee"
        , "12"
        , "--tx-body-file"
        , txBodyOutFile
        ]

    goldenFile <- H.note "test/cardano-cli-golden/files/golden/shelley/build-raw-tx-body-out-2.json"
    H.diffFileVsGoldenFile txBodyOutFile goldenFile

hprop_golden_shelley_transaction_build_minting :: Property
hprop_golden_shelley_transaction_build_minting =
  propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    let scriptWit = "test/cardano-cli-golden/files/input/shelley/multisig/scripts/any"

    polid <-
      execCardanoCLI
        [ "latest"
        , "transaction"
        , "policyid"
        , "--script-file"
        , scriptWit
        ]

    let dummyMA =
          filter (/= '\n') $
            "50 " ++ polid ++ "." ++ BSC.unpack (Base16.encode "ethereum")

    txBodyOutFile <- noteTempFile tempDir "tx-body-out"

    void $
      execCardanoCLI
        [ "mary"
        , "transaction"
        , "build-raw"
        , "--tx-in"
        , txIn
        , "--tx-out"
        , txOut ++ "+" ++ dummyMA
        , "--mint-script-file"
        , scriptWit
        , "--mint"
        , dummyMA
        , "--fee"
        , "12"
        , "--tx-body-file"
        , txBodyOutFile
        ]

    goldenFile <- H.note "test/cardano-cli-golden/files/golden/shelley/build-raw-tx-body-out-3.json"
    H.diffFileVsGoldenFile txBodyOutFile goldenFile

hprop_golden_shelley_transaction_build_withdrawal_script_witnessed :: Property
hprop_golden_shelley_transaction_build_withdrawal_script_witnessed =
  propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    txBodyOutFile <- noteTempFile tempDir "tx-body-out"

    stakeAddress <-
      H.readFile "test/cardano-cli-golden/files/input/shelley/keys/stake_keys/reward_address"

    let withdrawal = filter (/= '\n') $ stakeAddress <> "+100"
        scriptWit = "test/cardano-cli-golden/files/input/shelley/multisig/scripts/any"

    void $
      execCardanoCLI
        [ "mary"
        , "transaction"
        , "build-raw"
        , "--tx-in"
        , txIn
        , "--tx-out"
        , txOut
        , "--withdrawal"
        , withdrawal
        , "--withdrawal-script-file"
        , scriptWit
        , "--fee"
        , "12"
        , "--tx-body-file"
        , txBodyOutFile
        ]

    goldenFile <- H.note "test/cardano-cli-golden/files/golden/shelley/build-raw-tx-body-out-4.json"
    H.diffFileVsGoldenFile txBodyOutFile goldenFile

hprop_golden_shelley_transaction_build_txin_script_witnessed :: Property
hprop_golden_shelley_transaction_build_txin_script_witnessed =
  propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    let scriptWit = "test/cardano-cli-golden/files/input/shelley/multisig/scripts/any"

    txBodyOutFile <- noteTempFile tempDir "tx-body-out"

    void $
      execCardanoCLI
        [ "mary"
        , "transaction"
        , "build-raw"
        , "--tx-in"
        , txIn
        , "--txin-script-file"
        , scriptWit
        , "--tx-out"
        , txOut
        , "--fee"
        , "12"
        , "--tx-body-file"
        , txBodyOutFile
        ]

    goldenFile <- H.note "test/cardano-cli-golden/files/golden/shelley/build-raw-tx-body-out-5.json"
    H.diffFileVsGoldenFile txBodyOutFile goldenFile

hprop_golden_shelley_transaction_build_txout_inline_datum :: Property
hprop_golden_shelley_transaction_build_txout_inline_datum =
  propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    let datum = "test/cardano-cli-golden/files/input/babbage/42.datum"

    txBodyOutFile <- noteTempFile tempDir "tx-body-out"

    void $
      execCardanoCLI
        [ "babbage"
        , "transaction"
        , "build-raw"
        , "--tx-in"
        , txIn
        , "--tx-out"
        , txOut
        , "--tx-out-inline-datum-file"
        , datum
        , "--fee"
        , "12"
        , "--tx-body-file"
        , txBodyOutFile
        ]

    goldenFile <- H.note "test/cardano-cli-golden/files/golden/shelley/build-raw-tx-body-out-6.json"
    H.diffFileVsGoldenFile txBodyOutFile goldenFile
