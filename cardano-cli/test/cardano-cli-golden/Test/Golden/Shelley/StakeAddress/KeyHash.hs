{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.StakeAddress.KeyHash where

import Test.Cardano.CLI.Util as OP

import Hedgehog (Property)
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.Golden qualified as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelleyStakeAddressKeyHash :: Property
hprop_golden_shelleyStakeAddressKeyHash = propertyOnce . H.moduleWorkspace "tmp" $ \_ -> do
  verificationKeyFile <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/stake_keys/verification_key"
  goldenVerificationKeyHashFile <-
    noteInputFile
      "test/cardano-cli-golden/files/input/shelley/keys/stake_keys/verification_key.key-hash"

  verificationKeyHash <-
    execCardanoCLI
      [ "latest"
      , "stake-address"
      , "key-hash"
      , "--stake-verification-key-file"
      , verificationKeyFile
      ]

  H.diffVsGoldenFile verificationKeyHash goldenVerificationKeyHashFile
