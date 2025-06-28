{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.StakeAddress.KeyGen where

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog.Extras (UnitIO)
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.File qualified as H

{- HLINT ignore "Use camelCase" -}

tasty_golden_shelleyStakeAddressKeyGen :: UnitIO ()
tasty_golden_shelleyStakeAddressKeyGen =
  H.moduleWorkspace "tmp" $ \tempDir -> do
    verificationKeyFile <- noteTempFile tempDir "kes.vkey"
    signingKeyFile <- noteTempFile tempDir "kes.skey"

    void $
      execCardanoCLI
        [ "latest"
        , "stake-address"
        , "key-gen"
        , "--verification-key-file"
        , verificationKeyFile
        , "--signing-key-file"
        , signingKeyFile
        ]

    H.assertFileOccurences 1 "StakeVerificationKeyShelley_ed25519" verificationKeyFile
    H.assertFileOccurences 1 "StakeSigningKeyShelley_ed25519" signingKeyFile
