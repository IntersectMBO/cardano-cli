{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Genesis.KeyGenGenesis where

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.File qualified as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelleyGenesisKeyGenGenesis :: Property
hprop_golden_shelleyGenesisKeyGenGenesis = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
  signingKeyFile <- noteTempFile tempDir "key-gen.skey"

  void $
    execCardanoCLI
      [ "genesis"
      , "key-gen-genesis"
      , "--verification-key-file"
      , verificationKeyFile
      , "--signing-key-file"
      , signingKeyFile
      ]

  H.assertFileOccurences 1 "GenesisVerificationKey_ed25519" verificationKeyFile
  H.assertFileOccurences 1 "GenesisSigningKey_ed25519" signingKeyFile

  H.assertEndsWithSingleNewline verificationKeyFile
  H.assertEndsWithSingleNewline signingKeyFile
