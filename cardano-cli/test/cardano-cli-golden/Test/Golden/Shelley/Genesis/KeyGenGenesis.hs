{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Genesis.KeyGenGenesis where

import           Control.Monad (void)

import           Test.Cardano.CLI.Aeson
import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelleyGenesisKeyGenGenesis :: Property
hprop_golden_shelleyGenesisKeyGenGenesis = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
  signingKeyFile <- noteTempFile tempDir "key-gen.skey"

  void $
    execCardanoCLI
      [ "latest"
      , "genesis"
      , "key-gen-genesis"
      , "--verification-key-file"
      , verificationKeyFile
      , "--signing-key-file"
      , signingKeyFile
      ]

  assertHasMappings [("type", "GenesisVerificationKey_ed25519")] verificationKeyFile
  assertHasMappings [("type", "GenesisSigningKey_ed25519")] signingKeyFile

  H.assertEndsWithSingleNewline verificationKeyFile
  H.assertEndsWithSingleNewline signingKeyFile
