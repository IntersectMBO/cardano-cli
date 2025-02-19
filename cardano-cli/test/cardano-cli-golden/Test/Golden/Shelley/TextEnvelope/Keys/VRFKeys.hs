{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Golden.Shelley.TextEnvelope.Keys.VRFKeys where

import Cardano.Api (AsType (..), HasTextEnvelope (..))

import Control.Monad (void)
import Text.Regex.TDFA ((=~))

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog qualified as H
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.File qualified as H

{- HLINT ignore "Use camelCase" -}

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
hprop_golden_shelleyVRFKeys :: Property
hprop_golden_shelleyVRFKeys = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  H.note_ tempDir

  -- Reference keys
  referenceVerKey <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/vrf_keys/verification_key"
  referenceSignKey <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/vrf_keys/signing_key"

  -- Key filepaths
  verKey <- noteTempFile tempDir "vrf-verification-key-file"
  signKey <- noteTempFile tempDir "vrf-signing-key-file"

  -- Generate vrf verification key
  void $
    execCardanoCLI
      [ "latest"
      , "node"
      , "key-gen-VRF"
      , "--verification-key-file"
      , verKey
      , "--signing-key-file"
      , signKey
      ]

  let signingKeyType = textEnvelopeType (AsSigningKey AsVrfKey)
      verificationKeyType = textEnvelopeType (AsVerificationKey AsVrfKey)

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
  checkTextEnvelopeFormat signingKeyType referenceSignKey signKey

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
hprop_golden_shelleyVRFKeys_te :: Property
hprop_golden_shelleyVRFKeys_te = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  H.note_ tempDir

  -- Reference keys
  referenceVerKey <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/vrf_keys/verification_key"
  referenceSignKey <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/vrf_keys/signing_key"

  -- Key filepaths
  verKey <- noteTempFile tempDir "vrf-verification-key-file"
  signKey <- noteTempFile tempDir "vrf-signing-key-file"

  -- Generate vrf verification key
  void $
    execCardanoCLI
      [ "latest"
      , "node"
      , "key-gen-VRF"
      , "--key-output-format"
      , "text-envelope"
      , "--verification-key-file"
      , verKey
      , "--signing-key-file"
      , signKey
      ]

  let signingKeyType = textEnvelopeType (AsSigningKey AsVrfKey)
      verificationKeyType = textEnvelopeType (AsVerificationKey AsVrfKey)

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
  checkTextEnvelopeFormat signingKeyType referenceSignKey signKey

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the bech32 serialization format has not changed.
hprop_golden_shelleyVRFKeys_bech32 :: Property
hprop_golden_shelleyVRFKeys_bech32 = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  H.note_ tempDir

  -- Key filepaths
  verKeyFile <- noteTempFile tempDir "vrf-verification-key-file"
  signKeyFile <- noteTempFile tempDir "vrf-signing-key-file"

  -- Generate vrf verification key
  void $
    execCardanoCLI
      [ "latest"
      , "node"
      , "key-gen-VRF"
      , "--key-output-format"
      , "bech32"
      , "--verification-key-file"
      , verKeyFile
      , "--signing-key-file"
      , signKeyFile
      ]

  verKey <- H.readFile verKeyFile
  H.assert $ verKey =~ id @String "vrf_vk[a-z0-9]{59}"

  signKey <- H.readFile signKeyFile
  H.assert $ signKey =~ id @String "vrf_sk[a-z0-9]{110}"
