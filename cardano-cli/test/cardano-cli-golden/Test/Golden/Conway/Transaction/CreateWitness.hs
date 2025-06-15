{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Conway.Transaction.CreateWitness where

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog.Extras (UnitIO)
import Hedgehog.Extras.Test qualified as H

{- HLINT ignore "Use camelCase" -}

txIn :: String
txIn = "2392d2b1200b5139fe555c81261697b29a8ccf561c5c783d46e78a479d977053#0"

txOut :: String
txOut =
  "addr1q94cxl99qvtwunsqqv6g9mgj3zrawtpt4edsgwxkjtwpy5dsezcht90tmwfur7t5hc9fk8hjd3r5vjwec2h8vmk3xh8s7er7t3+100"

tasty_golden_shelley_transaction_signing_key_witness :: UnitIO ()
tasty_golden_shelley_transaction_signing_key_witness =
  H.moduleWorkspace "tmp" $ \tempDir -> do
    txBodyOutFile <- noteTempFile tempDir "tx-body-out"

    -- Create tx body file
    void $
      execCardanoCLI
        [ "conway"
        , "transaction"
        , "build-raw"
        , "--tx-in"
        , txIn
        , "--tx-out"
        , txOut
        , "--invalid-hereafter"
        , "60"
        , "--fee"
        , "12"
        , "--tx-body-file"
        , txBodyOutFile
        ]

    -- Create all multisig witness
    witnessOutFile <- noteTempFile tempDir "signingkey-witness"
    signingKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/conway/keys/payment_keys/signing_key"

    void $
      execCardanoCLI
        [ "latest"
        , "transaction"
        , "witness"
        , "--tx-body-file"
        , txBodyOutFile
        , "--signing-key-file"
        , signingKeyFile
        , "--mainnet"
        , "--out-file"
        , witnessOutFile
        ]

    goldenFile <- H.note "test/cardano-cli-golden/files/golden/conway/witness-out.json"
    H.diffFileVsGoldenFile witnessOutFile goldenFile
