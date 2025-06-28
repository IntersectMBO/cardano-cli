{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.StakeAddress.KeyGen where

import Control.Monad (void)

import Test.Cardano.CLI.Util
import Test.Cardano.CLI.Workspace

import Hedgehog (Property)
import Hedgehog.Extras.Test.File qualified as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelleyStakeAddressKeyGen :: Property
hprop_golden_shelleyStakeAddressKeyGen =
  watchdogProp . propertyOnce . moduleWorkspace2 "tmp" $ \tempDir -> do
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
