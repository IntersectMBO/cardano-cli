{-# LANGUAGE OverloadedStrings #-}

module Test.Cli.Pioneers.Exercise6
  ( hprop_createZeroLovelaceTxOutTransaction
  )
where

import Control.Monad (void)

import Test.Cardano.CLI.Hedgehog qualified as H
import Test.Cardano.CLI.Util

import Hedgehog (Property)

-- | 1. We generate a payment signing key
--   2. We create a tx body
--   3. We sign the tx body with the generated payment signing key
hprop_createZeroLovelaceTxOutTransaction :: Property
hprop_createZeroLovelaceTxOutTransaction =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- Key filepaths
    paymentVerKey <- noteTempFile tempDir "payment-verification-key-file"
    paymentSignKey <- noteTempFile tempDir "payment-signing-key-file"
    transactionBodyFile <- noteTempFile tempDir "transaction-body"
    transactionFile <- noteTempFile tempDir "transaction-file"

    -- Generate payment signing key to sign transaction
    void $
      execCardanoCLI
        [ "latest"
        , "address"
        , "key-gen"
        , "--verification-key-file"
        , paymentVerKey
        , "--signing-key-file"
        , paymentSignKey
        ]

    H.assertFilesExist [paymentVerKey, paymentSignKey]

    -- Create transaction body
    void $
      execCardanoCLI
        [ "latest"
        , "transaction"
        , "build-raw"
        , "--tx-in"
        , "91999ea21177b33ebe6b8690724a0c026d410a11ad7521caa350abdafa5394c3#0"
        , "--tx-out"
        , "addr1v9wmu83pzajplrtpsq6tsqdgwr98x888trpmah2u0ezznsge7del3 0 lovelace"
        , "--fee"
        , "1000000"
        , "--invalid-hereafter"
        , "500000"
        , "--out-file"
        , transactionBodyFile
        ]

    H.assertFilesExist [transactionBodyFile]

    -- Sign transaction
    void $
      execCardanoCLI
        [ "latest"
        , "transaction"
        , "sign"
        , "--tx-body-file"
        , transactionBodyFile
        , "--signing-key-file"
        , paymentSignKey
        , "--mainnet"
        , "--out-file"
        , transactionFile
        ]

    H.assertFilesExist [paymentVerKey, paymentSignKey, transactionBodyFile, transactionFile]
