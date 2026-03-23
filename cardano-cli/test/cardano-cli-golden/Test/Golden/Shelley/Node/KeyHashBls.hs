{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Node.KeyHashBls where

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog.Extras.Test qualified as H

hprop_golden_shelleyNodeKeyHashBls :: Property
hprop_golden_shelleyNodeKeyHashBls =
  watchdogProp . propertyOnce $ do
    verificationKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/bls_keys/verification_key"
    goldenVerificationKeyHashFile <-
      noteInputFile
        "test/cardano-cli-golden/files/input/shelley/keys/bls_keys/verification_key.key-hash"

    verificationKeyHash <-
      execCardanoCLI
        [ "latest"
        , "node"
        , "key-hash-BLS"
        , "--verification-key-file"
        , verificationKeyFile
        ]

    H.diffVsGoldenFile verificationKeyHash goldenVerificationKeyHashFile
