{-# LANGUAGE OverloadedStrings #-}

module Test.Cli.Pioneers.Exercise1
  ( hprop_buildShelleyPaymentAddress
  , hprop_buildShelleyStakeAddress
  )
where

import Control.Monad (void)

import Test.Cardano.CLI.Hedgehog qualified as H
import Test.Cardano.CLI.Util

import Hedgehog (Property)

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. We use the generated verification key to build a shelley payment address.
hprop_buildShelleyPaymentAddress :: Property
hprop_buildShelleyPaymentAddress =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- Key filepaths
    verKey <- noteTempFile tempDir "payment-verification-key-file"
    signKey <- noteTempFile tempDir "payment-signing-key-file"

    -- Generate payment verification key
    void $
      execCardanoCLI
        [ "latest"
        , "address"
        , "key-gen"
        , "--verification-key-file"
        , verKey
        , "--signing-key-file"
        , signKey
        ]

    H.assertFilesExist [verKey, signKey]

    -- Build shelley payment address
    void $
      execCardanoCLI
        [ "latest"
        , "address"
        , "build"
        , "--payment-verification-key-file"
        , verKey
        , "--mainnet"
        ]

-- | 1. We generate a key payment pair
--   2. We generate a staking key pair
--   2. Check for the existence of the key pairs
--   3. We use the payment verification key & staking verification key
--      to build a shelley stake address.
hprop_buildShelleyStakeAddress :: Property
hprop_buildShelleyStakeAddress =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- Key filepaths
    stakeVerKey <- noteTempFile tempDir "stake-verification-key-file"
    stakeSignKey <- noteTempFile tempDir "stake-signing-key-file"
    paymentVerKey <- noteTempFile tempDir "payment-verification-key-file"
    paymentSignKey <- noteTempFile tempDir "payment-signing-key-file"

    -- Generate payment verification key
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

    -- Generate stake verification key
    void $
      execCardanoCLI
        [ "latest"
        , "stake-address"
        , "key-gen"
        , "--verification-key-file"
        , stakeVerKey
        , "--signing-key-file"
        , stakeSignKey
        ]

    H.assertFilesExist [stakeVerKey, stakeSignKey, paymentVerKey, paymentSignKey]

    -- Build shelley stake address
    void $
      execCardanoCLI
        [ "latest"
        , "address"
        , "build"
        , "--payment-verification-key-file"
        , paymentVerKey
        , "--stake-verification-key-file"
        , stakeVerKey
        , "--mainnet"
        ]
