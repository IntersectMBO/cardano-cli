{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Address.KeyGen where

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelleyAddressKeyGen :: Property
hprop_golden_shelleyAddressKeyGen = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  addressVKeyFile <- noteTempFile tempDir "address.vkey"
  addressSKeyFile <- noteTempFile tempDir "address.skey"

  void $
    execCardanoCLI
      [ "address"
      , "key-gen"
      , "--verification-key-file"
      , addressVKeyFile
      , "--signing-key-file"
      , addressSKeyFile
      ]

  void $ H.readFile addressVKeyFile
  void $ H.readFile addressSKeyFile

  H.assertFileOccurences 1 "PaymentVerificationKeyShelley_ed25519" addressVKeyFile
  H.assertFileOccurences 1 "PaymentSigningKeyShelley_ed25519" addressSKeyFile

hprop_golden_shelleyAddressExtendedKeyGen :: Property
hprop_golden_shelleyAddressExtendedKeyGen = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  addressVKeyFile <- noteTempFile tempDir "address.vkey"
  addressSKeyFile <- noteTempFile tempDir "address.skey"

  void $
    execCardanoCLI
      [ "address"
      , "key-gen"
      , "--extended-key"
      , "--verification-key-file"
      , addressVKeyFile
      , "--signing-key-file"
      , addressSKeyFile
      ]

  void $ H.readFile addressVKeyFile
  void $ H.readFile addressSKeyFile

  H.assertFileOccurences 1 "PaymentExtendedVerificationKeyShelley_ed25519_bip32" addressVKeyFile
  H.assertFileOccurences 1 "PaymentExtendedSigningKeyShelley_ed25519_bip32" addressSKeyFile
