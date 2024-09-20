{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Golden.Shelley.TextEnvelope.Keys.StakeKeys where

import           Cardano.Api (AsType (..), HasTextEnvelope (..))

import           Control.Monad (void)
import           Text.Regex.TDFA ((=~))

import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
hprop_golden_shelleyStakeKeys :: Property
hprop_golden_shelleyStakeKeys = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceVerKey <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/stake_keys/verification_key"
  referenceSignKey <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/stake_keys/signing_key"

  -- Key filepaths
  verKey <- noteTempFile tempDir "stake-verification-key-file"
  signKey <- noteTempFile tempDir "stake-signing-key-file"

  -- Generate stake key pair
  void $
    execCardanoCLI
      [ "latest"
      , "stake-address"
      , "key-gen"
      , "--verification-key-file"
      , verKey
      , "--signing-key-file"
      , signKey
      ]

  let signingKeyType = textEnvelopeType (AsSigningKey AsStakeKey)
      verificationKeyType = textEnvelopeType (AsVerificationKey AsStakeKey)

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
  checkTextEnvelopeFormat signingKeyType referenceSignKey signKey

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
hprop_golden_shelleyStakeKeys_te :: Property
hprop_golden_shelleyStakeKeys_te = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceVerKey <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/stake_keys/verification_key"
  referenceSignKey <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/stake_keys/signing_key"

  -- Key filepaths
  verKey <- noteTempFile tempDir "stake-verification-key-file"
  signKey <- noteTempFile tempDir "stake-signing-key-file"

  -- Generate stake key pair
  void $
    execCardanoCLI
      [ "latest"
      , "stake-address"
      , "key-gen"
      , "--key-output-format"
      , "text-envelope"
      , "--verification-key-file"
      , verKey
      , "--signing-key-file"
      , signKey
      ]

  let signingKeyType = textEnvelopeType (AsSigningKey AsStakeKey)
      verificationKeyType = textEnvelopeType (AsVerificationKey AsStakeKey)

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
  checkTextEnvelopeFormat signingKeyType referenceSignKey signKey

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the bech32 serialization format has not changed.
hprop_golden_shelleyStakeKeys_bech32 :: Property
hprop_golden_shelleyStakeKeys_bech32 = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  H.note_ tempDir

  -- Key filepaths
  verKeyFile <- noteTempFile tempDir "stake-verification-key-file"
  signKeyFile <- noteTempFile tempDir "stake-signing-key-file"

  -- Generate stake key pair
  void $
    execCardanoCLI
      [ "latest"
      , "stake-address"
      , "key-gen"
      , "--key-output-format"
      , "bech32"
      , "--verification-key-file"
      , verKeyFile
      , "--signing-key-file"
      , signKeyFile
      ]

  verKey <- H.readFile verKeyFile
  H.assert $ verKey =~ id @String "stake_vk[a-z0-9]{59}"

  signKey <- H.readFile signKeyFile
  H.assert $ signKey =~ id @String "stake_sk[a-z0-9]{59}"
