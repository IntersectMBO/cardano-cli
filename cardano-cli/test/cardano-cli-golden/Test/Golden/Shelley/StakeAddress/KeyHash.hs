{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.StakeAddress.KeyHash where

import Test.Cardano.CLI.Util as OP

import Hedgehog.Extras (UnitIO)
import Hedgehog.Extras.Test qualified as H

{- HLINT ignore "Use camelCase" -}

tasty_golden_shelleyStakeAddressKeyHash :: UnitIO ()
tasty_golden_shelleyStakeAddressKeyHash =
  H.moduleWorkspace "tmp" $ \_ -> do
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
