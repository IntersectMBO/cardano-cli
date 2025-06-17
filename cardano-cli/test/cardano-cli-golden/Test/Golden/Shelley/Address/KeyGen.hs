{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Address.KeyGen where

import Control.Monad (void)

import Test.Cardano.CLI.Aeson
import Test.Cardano.CLI.Util

import Hedgehog.Extras (UnitIO)
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.File qualified as H

{- HLINT ignore "Use camelCase" -}

tasty_golden_shelley_address_key_gen :: UnitIO ()
tasty_golden_shelley_address_key_gen =
  H.moduleWorkspace "tmp" $ \tempDir -> do
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

tasty_golden_shelley_address_extended_key_gen :: UnitIO ()
tasty_golden_shelley_address_extended_key_gen =
  H.moduleWorkspace "tmp" $ \tempDir -> do
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
