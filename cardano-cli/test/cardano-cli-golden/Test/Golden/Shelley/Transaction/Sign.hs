{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Transaction.Sign where

import           Control.Monad (void)

import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelleyTransactionSign :: Property
hprop_golden_shelleyTransactionSign = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  txBodyFile <- noteInputFile "test/cardano-cli-golden/files/input/shelley/tx/txbody"
  initialUtxo1SigningKeyFile <- noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/payment_keys/signing_key"
  utxoSigningKeyFile <- noteInputFile "test/cardano-cli-golden/files/input/shelley/transaction-sign/utxo.skey"
  stakeSigningKeyFile <- noteInputFile "test/cardano-cli-golden/files/input/shelley/transaction-sign/stake.skey"
  nodeColdSigningKeyFile <- noteInputFile "test/cardano-cli-golden/files/input/shelley/transaction-sign/node-cold.skey"
  ccHotSigningKeyFile <- noteInputFile "test/cardano-cli-golden/files/input/governance/cc-hot.skey"
  signedTransactionFile <- noteTempFile tempDir "signed.tx"
  transactionPoolRegSignedFile <- noteTempFile tempDir "tx-pool-reg.signed"

  -- Defaults to signing a Mainnet transaction

  void $ execCardanoCLI
    [ "transaction","sign"
    , "--mainnet"
    , "--tx-body-file", txBodyFile
    , "--signing-key-file", initialUtxo1SigningKeyFile
    , "--tx-file", signedTransactionFile
    ]

  H.assertFileOccurences 1 "Tx MaryEra" signedTransactionFile
  H.assertEndsWithSingleNewline signedTransactionFile

  -- Sign for a testnet with a testnet network magic of 11, but use two signing keys

  void $ execCardanoCLI
    [ "transaction","sign"
    , "--mainnet"
    , "--tx-body-file", txBodyFile
    , "--signing-key-file", initialUtxo1SigningKeyFile
    , "--signing-key-file", initialUtxo1SigningKeyFile
    , "--tx-file", signedTransactionFile
    ]

  H.assertFileOccurences 1 "Tx MaryEra" signedTransactionFile
  H.assertEndsWithSingleNewline signedTransactionFile

  -- Sign a pool registration transaction.
  -- TODO: This needs to use an unsigned tx with a registration certificate

  void $ execCardanoCLI
    [ "transaction","sign"
    , "--mainnet"
    , "--tx-body-file", txBodyFile
    , "--signing-key-file", utxoSigningKeyFile
    , "--signing-key-file", stakeSigningKeyFile
    , "--signing-key-file", nodeColdSigningKeyFile
    , "--tx-file", transactionPoolRegSignedFile
    ]

  H.assertFileOccurences 1 "Tx MaryEra" transactionPoolRegSignedFile
  H.assertEndsWithSingleNewline transactionPoolRegSignedFile

  void $ execCardanoCLI
    [ "transaction","sign"
    , "--mainnet"
    , "--tx-body-file", txBodyFile
    , "--signing-key-file", ccHotSigningKeyFile
    , "--tx-file", transactionPoolRegSignedFile
    ]

  H.assertFileOccurences 1 "Tx MaryEra" transactionPoolRegSignedFile
  H.assertEndsWithSingleNewline transactionPoolRegSignedFile
