{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Genesis.KeyGenUtxo where

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.File qualified as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelleyGenesisKeyGenUtxo :: Property
hprop_golden_shelleyGenesisKeyGenUtxo = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  utxoVerificationKeyFile <- noteTempFile tempDir "utxo.vkey"
  utxoSigningKeyFile <- noteTempFile tempDir "utxo.skey"

  void $
    execCardanoCLI
      [ "genesis"
      , "key-gen-utxo"
      , "--verification-key-file"
      , utxoVerificationKeyFile
      , "--signing-key-file"
      , utxoSigningKeyFile
      ]

  H.assertFileOccurences 1 "GenesisUTxOVerificationKey_ed25519" utxoVerificationKeyFile
  H.assertFileOccurences 1 "GenesisUTxOSigningKey_ed25519" utxoSigningKeyFile

  H.assertEndsWithSingleNewline utxoVerificationKeyFile
  H.assertEndsWithSingleNewline utxoSigningKeyFile
