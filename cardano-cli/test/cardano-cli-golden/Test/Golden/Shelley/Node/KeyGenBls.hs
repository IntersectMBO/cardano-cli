{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Node.KeyGenBls where

import Control.Monad (void)

import Test.Cardano.CLI.Aeson
import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.File qualified as H

hprop_golden_shelleyNodeKeyGenBls :: Property
hprop_golden_shelleyNodeKeyGenBls =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    verificationKey <- noteTempFile tempDir "bls.vkey"
    signingKey <- noteTempFile tempDir "bls.skey"

    void $
      execCardanoCLI
        [ "latest"
        , "node"
        , "key-gen-BLS"
        , "--verification-key-file"
        , verificationKey
        , "--signing-key-file"
        , signingKey
        ]

    assertHasMappings
      [ ("type", "BlsVerificationKey_bls12-381-BLS-Signature-Mininimal-Signature-Size")
      , ("description", "BLS12-381 verification key")
      ]
      verificationKey
    assertHasMappings
      [ ("type", "BlsSigningKey_bls12-381-BLS-Signature-Mininimal-Signature-Size")
      , ("description", "BLS12-381 signing key")
      ]
      signingKey

    H.assertEndsWithSingleNewline verificationKey
    H.assertEndsWithSingleNewline signingKey

hprop_golden_shelleyNodeKeyGenBls_te :: Property
hprop_golden_shelleyNodeKeyGenBls_te =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    verificationKey <- noteTempFile tempDir "bls.vkey"
    signingKey <- noteTempFile tempDir "bls.skey"

    void $
      execCardanoCLI
        [ "latest"
        , "node"
        , "key-gen-BLS"
        , "--key-output-format"
        , "text-envelope"
        , "--verification-key-file"
        , verificationKey
        , "--signing-key-file"
        , signingKey
        ]

    assertHasMappings
      [ ("type", "BlsVerificationKey_bls12-381-BLS-Signature-Mininimal-Signature-Size")
      , ("description", "BLS12-381 verification key")
      ]
      verificationKey
    assertHasMappings
      [ ("type", "BlsSigningKey_bls12-381-BLS-Signature-Mininimal-Signature-Size")
      , ("description", "BLS12-381 signing key")
      ]
      signingKey

    H.assertEndsWithSingleNewline verificationKey
    H.assertEndsWithSingleNewline signingKey

hprop_golden_shelleyNodeKeyGenBls_bech32 :: Property
hprop_golden_shelleyNodeKeyGenBls_bech32 =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    verificationKey <- noteTempFile tempDir "bls.vkey"
    signingKey <- noteTempFile tempDir "bls.skey"

    void $
      execCardanoCLI
        [ "latest"
        , "node"
        , "key-gen-BLS"
        , "--key-output-format"
        , "bech32"
        , "--verification-key-file"
        , verificationKey
        , "--signing-key-file"
        , signingKey
        ]

    H.assertFileOccurences 1 "bls_vk" verificationKey
    H.assertFileOccurences 1 "bls_sk" signingKey
