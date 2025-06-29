{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.TextEnvelope.Keys.GenesisDelegateKeys where

import Cardano.Api (AsType (..), HasTextEnvelope (..))

import Control.Monad (void)

import Test.Cardano.CLI.Hedgehog qualified as H
import Test.Cardano.CLI.Util

import Hedgehog (Property)

{- HLINT ignore "Use camelCase" -}

-- | 1. Generate a key pair & operational certificate counter file
--   2. Check for the existence of the key pair & counter file
--   3. Check the TextEnvelope serialization format has not changed.
hprop_golden_shelleyGenesisDelegateKeys :: Property
hprop_golden_shelleyGenesisDelegateKeys =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- Reference keys
    referenceVerKey <-
      noteInputFile
        "test/cardano-cli-golden/files/input/shelley/keys/genesis_delegate_keys/verification_key"
    referenceSignKey <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/genesis_delegate_keys/signing_key"
    referenceOpCertCounter <-
      noteInputFile
        "test/cardano-cli-golden/files/input/shelley/keys/genesis_delegate_keys/operational_certificate_counter"

    -- Key filepaths
    verKey <- noteTempFile tempDir "genesis-delegate-verification-key-file"
    signKey <- noteTempFile tempDir "genesis-delegate-signing-key-file"
    opCertCounter <- noteTempFile tempDir "delegate-operational-cert-counter-file"

    -- Generate payment verification key
    void $
      execCardanoCLI
        [ "latest"
        , "genesis"
        , "key-gen-delegate"
        , "--verification-key-file"
        , verKey
        , "--signing-key-file"
        , signKey
        , "--operational-certificate-issue-counter-file"
        , opCertCounter
        ]

    let signingKeyType = textEnvelopeType (AsSigningKey AsGenesisDelegateKey)
        verificationKeyType = textEnvelopeType (AsVerificationKey AsGenesisDelegateKey)
        operationalCertCounterType = textEnvelopeType AsOperationalCertificateIssueCounter

    -- Check the newly created files have not deviated from the
    -- golden files
    checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
    checkTextEnvelopeFormat signingKeyType referenceSignKey signKey
    checkTextEnvelopeFormat operationalCertCounterType referenceOpCertCounter opCertCounter
