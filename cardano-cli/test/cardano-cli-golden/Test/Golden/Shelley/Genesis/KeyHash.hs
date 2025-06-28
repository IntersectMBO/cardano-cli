{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Genesis.KeyHash where

import Test.Cardano.CLI.Util as OP
import Test.Cardano.CLI.Workspace

import Hedgehog (Property, (===))
import Hedgehog.Extras.Test.File qualified as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelleyGenesisKeyHash :: Property
hprop_golden_shelleyGenesisKeyHash =
  watchdogProp . propertyOnce . moduleWorkspace2 "tmp" $ \tempDir -> do
    referenceVerificationKey <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/genesis_keys/verification_key"
    goldenGenesisVerificationKeyHashFile <-
      noteInputFile
        "test/cardano-cli-golden/files/input/shelley/keys/genesis_keys/verification_key.key-hash"
    genesisVerificationKeyHashFile <- noteTempFile tempDir "key-hash.hex"

    genesisVerificationKeyHash <-
      execCardanoCLI
        [ "latest"
        , "genesis"
        , "key-hash"
        , "--verification-key-file"
        , referenceVerificationKey
        ]

    H.writeFile genesisVerificationKeyHashFile genesisVerificationKeyHash

    goldenGenesisVerificationKeyHash <- H.readFile goldenGenesisVerificationKeyHashFile

    genesisVerificationKeyHash === goldenGenesisVerificationKeyHash
