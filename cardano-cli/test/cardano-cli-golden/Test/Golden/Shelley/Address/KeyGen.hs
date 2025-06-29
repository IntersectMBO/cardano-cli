{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Address.KeyGen where

import Control.Monad (void)

import Test.Cardano.CLI.Aeson
import Test.Cardano.CLI.Hedgehog qualified as H
import Test.Cardano.CLI.Util

import Hedgehog (Property)

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelley_address_key_gen :: Property
hprop_golden_shelley_address_key_gen =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    addressVKeyFile <- noteTempFile tempDir "address.vkey"
    addressSKeyFile <- noteTempFile tempDir "address.skey"

    void $
      execCardanoCLI
        [ "latest"
        , "address"
        , "key-gen"
        , "--verification-key-file"
        , addressVKeyFile
        , "--signing-key-file"
        , addressSKeyFile
        ]

    assertHasMappings
      [ ("type", "PaymentVerificationKeyShelley_ed25519")
      , ("description", "Payment Verification Key")
      ]
      addressVKeyFile
    assertHasKeys ["cborHex"] addressVKeyFile
    H.assertEndsWithSingleNewline addressVKeyFile

    assertHasMappings
      [ ("type", "PaymentSigningKeyShelley_ed25519")
      , ("description", "Payment Signing Key")
      ]
      addressSKeyFile
    assertHasKeys ["cborHex"] addressSKeyFile
    H.assertEndsWithSingleNewline addressSKeyFile

hprop_golden_shelley_address_extended_key_gen :: Property
hprop_golden_shelley_address_extended_key_gen =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    addressVKeyFile <- noteTempFile tempDir "address.vkey"
    addressSKeyFile <- noteTempFile tempDir "address.skey"

    void $
      execCardanoCLI
        [ "latest"
        , "address"
        , "key-gen"
        , "--extended-key"
        , "--verification-key-file"
        , addressVKeyFile
        , "--signing-key-file"
        , addressSKeyFile
        ]

    assertHasMappings
      [ ("type", "PaymentExtendedVerificationKeyShelley_ed25519_bip32")
      , ("description", "Payment Verification Key")
      ]
      addressVKeyFile
    assertHasKeys ["cborHex"] addressVKeyFile
    H.assertEndsWithSingleNewline addressVKeyFile

    assertHasMappings
      [ ("type", "PaymentExtendedSigningKeyShelley_ed25519_bip32")
      , ("description", "Payment Signing Key")
      ]
      addressSKeyFile
    assertHasKeys ["cborHex"] addressSKeyFile
    H.assertEndsWithSingleNewline addressSKeyFile
