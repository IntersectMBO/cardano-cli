{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Golden.Shelley.TextEnvelope.Keys.PaymentKeys where

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
hprop_golden_shelleyPaymentKeys :: Property
hprop_golden_shelleyPaymentKeys = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceVerKey <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/payment_keys/verification_key"
  referenceSignKey <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/payment_keys/signing_key"

  -- Key filepaths
  verKey <- noteTempFile tempDir "payment-verification-key-file"
  signKey <- noteTempFile tempDir "payment-signing-key-file"

  -- Generate payment verification key
  void $
    execCardanoCLI
      [ "address"
      , "key-gen"
      , "--verification-key-file"
      , verKey
      , "--signing-key-file"
      , signKey
      ]

  let signingKeyType = textEnvelopeType (AsSigningKey AsPaymentKey)
      verificationKeyType = textEnvelopeType (AsVerificationKey AsPaymentKey)

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
  checkTextEnvelopeFormat signingKeyType referenceSignKey signKey

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
hprop_golden_shelleyPaymentKeys_te :: Property
hprop_golden_shelleyPaymentKeys_te = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceVerKey <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/payment_keys/verification_key"
  referenceSignKey <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/payment_keys/signing_key"

  -- Key filepaths
  verKey <- noteTempFile tempDir "payment-verification-key-file"
  signKey <- noteTempFile tempDir "payment-signing-key-file"

  -- Generate payment verification key
  void $
    execCardanoCLI
      [ "address"
      , "key-gen"
      , "--key-output-format"
      , "text-envelope"
      , "--verification-key-file"
      , verKey
      , "--signing-key-file"
      , signKey
      ]

  let signingKeyType = textEnvelopeType (AsSigningKey AsPaymentKey)
      verificationKeyType = textEnvelopeType (AsVerificationKey AsPaymentKey)

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
  checkTextEnvelopeFormat signingKeyType referenceSignKey signKey

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the bech32 serialization format has not changed.
hprop_golden_shelleyPaymentKeys_bech32 :: Property
hprop_golden_shelleyPaymentKeys_bech32 = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  H.note_ tempDir

  -- Key filepaths
  verKeyFile <- noteTempFile tempDir "payment-verification-key-file"
  signKeyFile <- noteTempFile tempDir "payment-signing-key-file"

  -- Generate payment verification key
  void $
    execCardanoCLI
      [ "address"
      , "key-gen"
      , "--key-output-format"
      , "bech32"
      , "--verification-key-file"
      , verKeyFile
      , "--signing-key-file"
      , signKeyFile
      ]

  verKey <- H.readFile verKeyFile
  H.assert $ verKey =~ id @String "^addr_vk[a-z0-9]{59}$"

  signKey <- H.readFile signKeyFile
  H.assert $ signKey =~ id @String "^addr_sk[a-z0-9]{59}$"
