{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Node.KeyGenVrf where

import Control.Monad (void)

import Test.Cardano.CLI.Aeson
import Test.Cardano.CLI.Hedgehog qualified as H
import Test.Cardano.CLI.Util

import Hedgehog (Property)

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelleyNodeKeyGenVrf :: Property
hprop_golden_shelleyNodeKeyGenVrf =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    verificationKey <- noteTempFile tempDir "kes.vkey"
    signingKey <- noteTempFile tempDir "kes.skey"

    void $
      execCardanoCLI
        [ "latest"
        , "node"
        , "key-gen-VRF"
        , "--verification-key-file"
        , verificationKey
        , "--signing-key-file"
        , signingKey
        ]

    assertHasMappings
      [("type", "VrfVerificationKey_PraosVRF"), ("description", "VRF Verification Key")]
      verificationKey
    assertHasMappings
      [("type", "VrfSigningKey_PraosVRF"), ("description", "VRF Signing Key")]
      signingKey

    H.assertEndsWithSingleNewline verificationKey
    H.assertEndsWithSingleNewline signingKey

hprop_golden_shelleyNodeKeyGenVrf_te :: Property
hprop_golden_shelleyNodeKeyGenVrf_te =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    verificationKey <- noteTempFile tempDir "kes.vkey"
    signingKey <- noteTempFile tempDir "kes.skey"

    void $
      execCardanoCLI
        [ "latest"
        , "node"
        , "key-gen-VRF"
        , "--key-output-format"
        , "text-envelope"
        , "--verification-key-file"
        , verificationKey
        , "--signing-key-file"
        , signingKey
        ]

    assertHasMappings
      [("type", "VrfVerificationKey_PraosVRF"), ("description", "VRF Verification Key")]
      verificationKey
    assertHasMappings
      [("type", "VrfSigningKey_PraosVRF"), ("description", "VRF Signing Key")]
      signingKey

    H.assertEndsWithSingleNewline verificationKey
    H.assertEndsWithSingleNewline signingKey

hprop_golden_shelleyNodeKeyGenVrf_bech32 :: Property
hprop_golden_shelleyNodeKeyGenVrf_bech32 =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    verificationKey <- noteTempFile tempDir "kes.vkey"
    signingKey <- noteTempFile tempDir "kes.skey"

    void $
      execCardanoCLI
        [ "latest"
        , "node"
        , "key-gen-VRF"
        , "--key-output-format"
        , "bech32"
        , "--verification-key-file"
        , verificationKey
        , "--signing-key-file"
        , signingKey
        ]

    H.assertFileOccurences 1 "vrf_vk" verificationKey
    H.assertFileOccurences 1 "vrf_sk" signingKey
