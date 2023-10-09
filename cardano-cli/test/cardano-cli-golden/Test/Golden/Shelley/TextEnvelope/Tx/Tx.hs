{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.TextEnvelope.Tx.Tx where

import           Control.Monad (void)

import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog.Extras.Test.Base as H

{- HLINT ignore "Use camelCase" -}

-- | 1. Generate a key pair
--   2. Create tx body
--   3. Sign tx body
--   4. Check the TextEnvelope serialization format has not changed.
hprop_golden_shelleyTx :: Property
hprop_golden_shelleyTx = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  let goldenReferenceTx = "test/cardano-cli-golden/files/golden/alonzo/tx"

  -- Key filepaths
  paymentSignKey <- noteInputFile "test/cardano-cli-golden/files/input/shelley/transaction-sign/utxo.skey"
  transactionFile <- noteTempFile tempDir "tx-file"
  transactionBodyFile <- noteTempFile tempDir "tx-body-file"

  -- Create transaction body
  void $ execCardanoCLI
    [ "alonzo", "transaction", "build-raw"
    , "--tx-in", "f62cd7bc15d8c6d2c8519fb8d13c57c0157ab6bab50af62bc63706feb966393d#0"
    , "--tx-out", "addr_test1qpmxr8d8jcl25kyz2tz9a9sxv7jxglhddyf475045y8j3zxjcg9vquzkljyfn3rasfwwlkwu7hhm59gzxmsyxf3w9dps8832xh+1199989833223"
    , "--tx-out", "addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4+10000000"
    , "--fee", "166777"
    , "--out-file", transactionBodyFile
    ]

  -- Sign transaction
  void $ execCardanoCLI
    [ "transaction", "sign"
    , "--tx-body-file", transactionBodyFile
    , "--signing-key-file", paymentSignKey
    , "--testnet-magic", "42"
    , "--out-file", transactionFile
    ]

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTxCddlFormat goldenReferenceTx transactionFile

-- TODO Re-enable this test
disable_hprop_golden_checkIfConstitutionalCommitteeKeyCanSign :: Property
disable_hprop_golden_checkIfConstitutionalCommitteeKeyCanSign = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  let referenceTx = "test/cardano-cli-golden/files/input/conway/tx"

  -- Key filepaths
  paymentSignKey <- noteInputFile "test/cardano-cli-golden/files/input/shelley/transaction-sign/utxo.skey"
  -- constitutional committee signing key
  paymentSignKey2 <- noteInputFile "test/cardano-cli-golden/files/input/conway/cold1-cc.skey"
  transactionFile <- noteTempFile tempDir "tx-file"
  transactionBodyFile <- noteTempFile tempDir "tx-body-file"

  -- Create transaction body
  void $ execCardanoCLI
    [ "conway", "transaction", "build-raw"
    , "--tx-in", "f62cd7bc15d8c6d2c8519fb8d13c57c0157ab6bab50af62bc63706feb966393d#0"
    , "--tx-out", "addr_test1qpmxr8d8jcl25kyz2tz9a9sxv7jxglhddyf475045y8j3zxjcg9vquzkljyfn3rasfwwlkwu7hhm59gzxmsyxf3w9dps8832xh+1199989833223"
    , "--tx-out", "addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4+10000000"
    , "--fee", "166777"
    , "--out-file", transactionBodyFile
    ]

  -- Sign transaction
  void $ execCardanoCLI
    [ "transaction", "sign"
    , "--tx-body-file", transactionBodyFile
    , "--signing-key-file", paymentSignKey
    , "--signing-key-file", paymentSignKey2
    , "--testnet-magic", "42"
    , "--out-file", transactionFile
    ]

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTxCddlFormat referenceTx transactionFile
