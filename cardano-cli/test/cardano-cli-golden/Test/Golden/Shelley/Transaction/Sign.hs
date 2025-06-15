{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Transaction.Sign where

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog.Extras (UnitIO)
import Hedgehog.Extras.Test qualified as H

{- HLINT ignore "Use camelCase" -}

tasty_golden_shelley_transaction_sign :: UnitIO ()
tasty_golden_shelley_transaction_sign =
  H.moduleWorkspace "tmp" $ \tempDir -> do
    txBodyFile <- noteInputFile "test/cardano-cli-golden/files/input/shelley/tx/txbody"
    initialUtxo1SigningKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/payment_keys/signing_key"
    utxoSigningKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/transaction-sign/utxo.skey"
    stakeSigningKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/transaction-sign/stake.skey"
    nodeColdSigningKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/transaction-sign/node-cold.skey"
    ccHotSigningKeyFile <- noteInputFile "test/cardano-cli-golden/files/input/governance/cc-hot.skey"
    signedTransactionFile <- noteTempFile tempDir "signed.tx"
    transactionPoolRegSignedFile <- noteTempFile tempDir "tx-pool-reg.signed"

    -- Defaults to signing a Mainnet transaction

    void $
      execCardanoCLI
        [ "latest"
        , "transaction"
        , "sign"
        , "--mainnet"
        , "--tx-body-file"
        , txBodyFile
        , "--signing-key-file"
        , initialUtxo1SigningKeyFile
        , "--tx-file"
        , signedTransactionFile
        ]

    goldenFile1 <- H.note "test/cardano-cli-golden/files/golden/shelley/transaction-sign-1.json"
    H.diffFileVsGoldenFile signedTransactionFile goldenFile1

    -- Sign for a testnet with a testnet network magic of 11, but use two signing keys

    void $
      execCardanoCLI
        [ "latest"
        , "transaction"
        , "sign"
        , "--mainnet"
        , "--tx-body-file"
        , txBodyFile
        , "--signing-key-file"
        , initialUtxo1SigningKeyFile
        , "--signing-key-file"
        , initialUtxo1SigningKeyFile
        , "--tx-file"
        , signedTransactionFile
        ]

    H.diffFileVsGoldenFile signedTransactionFile goldenFile1

    -- Sign a pool registration transaction.
    -- TODO: This needs to use an unsigned tx with a registration certificate

    void $
      execCardanoCLI
        [ "latest"
        , "transaction"
        , "sign"
        , "--mainnet"
        , "--tx-body-file"
        , txBodyFile
        , "--signing-key-file"
        , utxoSigningKeyFile
        , "--signing-key-file"
        , stakeSigningKeyFile
        , "--signing-key-file"
        , nodeColdSigningKeyFile
        , "--tx-file"
        , transactionPoolRegSignedFile
        ]

    goldenFile2 <- H.note "test/cardano-cli-golden/files/golden/shelley/transaction-sign-2.json"
    H.diffFileVsGoldenFile transactionPoolRegSignedFile goldenFile2

    void $
      execCardanoCLI
        [ "latest"
        , "transaction"
        , "sign"
        , "--mainnet"
        , "--tx-body-file"
        , txBodyFile
        , "--signing-key-file"
        , ccHotSigningKeyFile
        , "--tx-file"
        , transactionPoolRegSignedFile
        ]

    goldenFile3 <- H.note "test/cardano-cli-golden/files/golden/shelley/transaction-sign-3.json"
    H.diffFileVsGoldenFile transactionPoolRegSignedFile goldenFile3
