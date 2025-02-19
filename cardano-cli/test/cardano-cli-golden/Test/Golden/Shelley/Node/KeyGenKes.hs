{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Node.KeyGenKes where

import Control.Monad (void)

import Test.Cardano.CLI.Aeson
import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.File qualified as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelleyNodeKeyGenKes :: Property
hprop_golden_shelleyNodeKeyGenKes = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKey <- noteTempFile tempDir "kes.vkey"
  signingKey <- noteTempFile tempDir "kes.skey"

  void $
    execCardanoCLI
      [ "latest"
      , "node"
      , "key-gen-KES"
      , "--verification-key-file"
      , verificationKey
      , "--signing-key-file"
      , signingKey
      ]

  assertHasMappings
    [("type", "KesVerificationKey_ed25519_kes_2^6"), ("description", "KES Verification Key")]
    verificationKey
  assertHasMappings
    [("type", "KesSigningKey_ed25519_kes_2^6"), ("description", "KES Signing Key")]
    signingKey

  H.assertEndsWithSingleNewline verificationKey
  H.assertEndsWithSingleNewline signingKey

hprop_golden_shelleyNodeKeyGenKes_te :: Property
hprop_golden_shelleyNodeKeyGenKes_te = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKey <- noteTempFile tempDir "kes.vkey"
  signingKey <- noteTempFile tempDir "kes.skey"

  void $
    execCardanoCLI
      [ "latest"
      , "node"
      , "key-gen-KES"
      , "--key-output-format"
      , "text-envelope"
      , "--verification-key-file"
      , verificationKey
      , "--signing-key-file"
      , signingKey
      ]

  assertHasMappings
    [("type", "KesVerificationKey_ed25519_kes_2^6"), ("description", "KES Verification Key")]
    verificationKey
  assertHasMappings
    [("type", "KesSigningKey_ed25519_kes_2^6"), ("description", "KES Signing Key")]
    signingKey

  H.assertEndsWithSingleNewline verificationKey
  H.assertEndsWithSingleNewline signingKey

hprop_golden_shelleyNodeKeyGenKes_bech32 :: Property
hprop_golden_shelleyNodeKeyGenKes_bech32 = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKey <- noteTempFile tempDir "kes.vkey"
  signingKey <- noteTempFile tempDir "kes.skey"

  void $
    execCardanoCLI
      [ "latest"
      , "node"
      , "key-gen-KES"
      , "--key-output-format"
      , "bech32"
      , "--verification-key-file"
      , verificationKey
      , "--signing-key-file"
      , signingKey
      ]

  H.assertFileOccurences 1 "kes_vk" verificationKey
  H.assertFileOccurences 1 "kes_sk" signingKey
