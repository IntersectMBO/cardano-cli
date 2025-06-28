{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Golden.Shelley.TextEnvelope.Keys.ExtendedPaymentKeys where

import Cardano.Api (AsType (..), HasTextEnvelope (..))

import Control.Monad (void)
import Text.Regex.TDFA ((=~))

import Test.Cardano.CLI.Util
import Test.Cardano.CLI.Workspace

import Hedgehog (Property)
import Hedgehog qualified as H
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.File qualified as H

{- HLINT ignore "Use camelCase" -}

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
hprop_golden_shelleyExtendedPaymentKeys :: Property
hprop_golden_shelleyExtendedPaymentKeys =
  watchdogProp . propertyOnce . moduleWorkspace2 "tmp" $ \tempDir -> do
    -- Reference keys
    referenceVerKey <-
      noteInputFile
        "test/cardano-cli-golden/files/input/shelley/keys/extended_payment_keys/verification_key"
    referenceSignKey <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/extended_payment_keys/signing_key"

    -- Key filepaths
    verKey <- noteTempFile tempDir "extended-payment-verification-key-file"
    signKey <- noteTempFile tempDir "extended-payment-signing-key-file"

    -- Generate payment verification key
    void $
      execCardanoCLI
        [ "latest"
        , "address"
        , "key-gen"
        , "--extended-key"
        , "--verification-key-file"
        , verKey
        , "--signing-key-file"
        , signKey
        ]

    let signingKeyType = textEnvelopeType (AsSigningKey AsPaymentExtendedKey)
        verificationKeyType = textEnvelopeType (AsVerificationKey AsPaymentExtendedKey)

    -- Check the newly created files have not deviated from the
    -- golden files
    checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
    checkTextEnvelopeFormat signingKeyType referenceSignKey signKey

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
hprop_golden_shelleyExtendedPaymentKeys_te :: Property
hprop_golden_shelleyExtendedPaymentKeys_te =
  watchdogProp . propertyOnce . moduleWorkspace2 "tmp" $ \tempDir -> do
    -- Reference keys
    referenceVerKey <-
      noteInputFile
        "test/cardano-cli-golden/files/input/shelley/keys/extended_payment_keys/verification_key"
    referenceSignKey <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/extended_payment_keys/signing_key"

    -- Key filepaths
    verKey <- noteTempFile tempDir "extended-payment-verification-key-file"
    signKey <- noteTempFile tempDir "extended-payment-signing-key-file"

    -- Generate payment verification key
    void $
      execCardanoCLI
        [ "latest"
        , "address"
        , "key-gen"
        , "--key-output-format"
        , "text-envelope"
        , "--extended-key"
        , "--verification-key-file"
        , verKey
        , "--signing-key-file"
        , signKey
        ]

    let signingKeyType = textEnvelopeType (AsSigningKey AsPaymentExtendedKey)
        verificationKeyType = textEnvelopeType (AsVerificationKey AsPaymentExtendedKey)

    -- Check the newly created files have not deviated from the
    -- golden files
    checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
    checkTextEnvelopeFormat signingKeyType referenceSignKey signKey

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the bech32 serialization format has not changed.
hprop_golden_shelleyExtendedPaymentKeys_bech32 :: Property
hprop_golden_shelleyExtendedPaymentKeys_bech32 =
  watchdogProp . propertyOnce . moduleWorkspace2 "tmp" $ \tempDir -> do
    H.note_ tempDir

    -- Key filepaths
    verKeyFile <- noteTempFile tempDir "payment-verification-key-file"
    signKeyFile <- noteTempFile tempDir "payment-signing-key-file"

    -- Generate payment verification key
    void $
      execCardanoCLI
        [ "latest"
        , "address"
        , "key-gen"
        , "--key-output-format"
        , "bech32"
        , "--extended-key"
        , "--verification-key-file"
        , verKeyFile
        , "--signing-key-file"
        , signKeyFile
        ]

    verKey <- H.readFile verKeyFile
    H.assert $ verKey =~ id @String "^addr_xvk[a-z0-9]{110}$"

    signKey <- H.readFile signKeyFile
    H.assert $ signKey =~ id @String "^addr_xsk[a-z0-9]{212}$"
