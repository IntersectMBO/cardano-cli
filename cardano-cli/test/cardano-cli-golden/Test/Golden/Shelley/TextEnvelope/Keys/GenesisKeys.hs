{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.TextEnvelope.Keys.GenesisKeys where

import Cardano.Api (AsType (..), HasTextEnvelope (..))

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog.Extras (UnitIO)
import Hedgehog.Extras.Test.Base qualified as H

{- HLINT ignore "Use camelCase" -}

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed
tasty_golden_shelleyGenesisKeys :: UnitIO ()
tasty_golden_shelleyGenesisKeys =
  H.moduleWorkspace "tmp" $ \tempDir -> do
    -- Reference keys
    referenceVerKey <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/genesis_keys/verification_key"
    referenceSignKey <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/genesis_keys/signing_key"

    -- Key filepaths
    verKey <- noteTempFile tempDir "genesis-verification-key-file"
    signKey <- noteTempFile tempDir "genesis-signing-key-file"

    -- Generate payment verification key
    void $
      execCardanoCLI
        [ "latest"
        , "genesis"
        , "key-gen-genesis"
        , "--verification-key-file"
        , verKey
        , "--signing-key-file"
        , signKey
        ]

    let signingKeyType = textEnvelopeType (AsSigningKey AsGenesisKey)
        verificationKeyType = textEnvelopeType (AsVerificationKey AsGenesisKey)

    -- Check the newly created files have not deviated from the
    -- golden files
    checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
    checkTextEnvelopeFormat signingKeyType referenceSignKey signKey
