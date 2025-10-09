{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Genesis.KeyGenUtxo where

import Control.Monad (void)

import Test.Cardano.CLI.Aeson
import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.File qualified as H

hprop_golden_shelleyGenesisKeyGenUtxo :: Property
hprop_golden_shelleyGenesisKeyGenUtxo =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    utxoVerificationKeyFile <- noteTempFile tempDir "utxo.vkey"
    utxoSigningKeyFile <- noteTempFile tempDir "utxo.skey"

    void $
      execCardanoCLI
        [ "latest"
        , "genesis"
        , "key-gen-utxo"
        , "--verification-key-file"
        , utxoVerificationKeyFile
        , "--signing-key-file"
        , utxoSigningKeyFile
        ]

    assertHasMappings [("type", "GenesisUTxOVerificationKey_ed25519")] utxoVerificationKeyFile
    assertHasMappings [("type", "GenesisUTxOSigningKey_ed25519")] utxoSigningKeyFile

    H.assertEndsWithSingleNewline utxoVerificationKeyFile
    H.assertEndsWithSingleNewline utxoSigningKeyFile
