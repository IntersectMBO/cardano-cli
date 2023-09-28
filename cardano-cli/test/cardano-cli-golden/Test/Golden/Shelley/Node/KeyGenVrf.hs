{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Node.KeyGenVrf where

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelleyNodeKeyGenVrf :: Property
hprop_golden_shelleyNodeKeyGenVrf = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKey <- noteTempFile tempDir "kes.vkey"
  signingKey <- noteTempFile tempDir "kes.skey"

  void $
    execCardanoCLI
      [ "node"
      , "key-gen-VRF"
      , "--verification-key-file"
      , verificationKey
      , "--signing-key-file"
      , signingKey
      ]

  H.assertFileOccurences 1 "VRF Verification Key" verificationKey
  H.assertFileOccurences 1 "VRF Signing Key" signingKey

  H.assertEndsWithSingleNewline verificationKey
  H.assertEndsWithSingleNewline signingKey

hprop_golden_shelleyNodeKeyGenVrf_te :: Property
hprop_golden_shelleyNodeKeyGenVrf_te = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKey <- noteTempFile tempDir "kes.vkey"
  signingKey <- noteTempFile tempDir "kes.skey"

  void $
    execCardanoCLI
      [ "node"
      , "key-gen-VRF"
      , "--key-output-format"
      , "text-envelope"
      , "--verification-key-file"
      , verificationKey
      , "--signing-key-file"
      , signingKey
      ]

  H.assertFileOccurences 1 "VRF Verification Key" verificationKey
  H.assertFileOccurences 1 "VRF Signing Key" signingKey

  H.assertEndsWithSingleNewline verificationKey
  H.assertEndsWithSingleNewline signingKey

hprop_golden_shelleyNodeKeyGenVrf_bech32 :: Property
hprop_golden_shelleyNodeKeyGenVrf_bech32 = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKey <- noteTempFile tempDir "kes.vkey"
  signingKey <- noteTempFile tempDir "kes.skey"

  void $
    execCardanoCLI
      [ "node"
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
