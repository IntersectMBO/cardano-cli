{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.TextEnvelope.Keys.GenesisUTxOKeys where

import Cardano.Api (AsType (..), HasTextEnvelope (..))

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog.Extras (UnitIO)
import Hedgehog.Extras.Test.Base qualified as H

{- HLINT ignore "Use camelCase" -}

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
tasty_golden_shelleyGenesisUTxOKeys :: UnitIO ()
tasty_golden_shelleyGenesisUTxOKeys =
  H.moduleWorkspace "tmp" $ \tempDir -> do
    -- Reference keys
    referenceVerKey <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/genesis_utxo_keys/verification_key"
    referenceSignKey <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/genesis_utxo_keys/signing_key"

    -- Key filepaths
    verKey <- noteTempFile tempDir "genesis-utxo-verification-key-file"
    signKey <- noteTempFile tempDir "genesis-utxo-signing-key-file"

    -- Generate payment verification key
    void $
      execCardanoCLI
        [ "latest"
        , "genesis"
        , "key-gen-utxo"
        , "--verification-key-file"
        , verKey
        , "--signing-key-file"
        , signKey
        ]

    let signingKeyType = textEnvelopeType (AsSigningKey AsGenesisUTxOKey)
        verificationKeyType = textEnvelopeType (AsVerificationKey AsGenesisUTxOKey)

    -- Check the newly created files have not deviated from the
    -- golden files
    checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
    checkTextEnvelopeFormat signingKeyType referenceSignKey signKey
