{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Transaction.CreateWitness where

import           Control.Monad (void)

import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

txIn :: String
txIn = "2392d2b1200b5139fe555c81261697b29a8ccf561c5c783d46e78a479d977053#0"

txOut :: String
txOut = "addr1q94cxl99qvtwunsqqv6g9mgj3zrawtpt4edsgwxkjtwpy5dsezcht90tmwfur7t5hc9fk8hjd3r5vjwec2h8vmk3xh8s7er7t3+100"

hprop_golden_shelleyTransactionSigningKeyWitness :: Property
hprop_golden_shelleyTransactionSigningKeyWitness = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  txBodyOutFile <- noteTempFile tempDir "tx-body-out"

  -- Create tx body file
  void $ execCardanoCLI
    [ "shelley", "transaction", "build-raw"
    , "--tx-in", txIn
    , "--tx-out", txOut
    , "--invalid-hereafter", "60"
    , "--fee", "12"
    , "--tx-body-file", txBodyOutFile
    ]

  -- Create all multisig witness
  witnessOutFile <- noteTempFile tempDir "signingkey-witness"
  signingKeyFile <- noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/payment_keys/signing_key"
  void $ execCardanoCLI
    [ "transaction","witness"
    , "--tx-body-file", txBodyOutFile
    , "--signing-key-file", signingKeyFile
    , "--mainnet"
    , "--out-file", witnessOutFile
    ]

  H.assertFileOccurences 1 "TxWitness ShelleyEra" witnessOutFile
  H.assertEndsWithSingleNewline txBodyOutFile
