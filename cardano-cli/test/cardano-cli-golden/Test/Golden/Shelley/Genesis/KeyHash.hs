{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Genesis.KeyHash where

import Test.Cardano.CLI.Util as OP

import Hedgehog ((===))
import Hedgehog.Extras (UnitIO)
import Hedgehog.Extras.Test qualified as H

{- HLINT ignore "Use camelCase" -}

tasty_golden_shelleyGenesisKeyHash :: UnitIO ()
tasty_golden_shelleyGenesisKeyHash =
  H.moduleWorkspace "tmp" $ \tempDir -> do
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
