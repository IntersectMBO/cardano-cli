{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.StakeAddress.Build where

import Test.Cardano.CLI.Util as OP

import Hedgehog (Property)
import Hedgehog.Extras.Test.File qualified as H

hprop_golden_shelleyStakeAddressBuild :: Property
hprop_golden_shelleyStakeAddressBuild =
  watchdogProp . propertyOnce $ do
    verificationKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/stake_keys/verification_key"
    goldenRewardAddressFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/stake_keys/reward_address"

    rewardAddress <-
      execCardanoCLI
        [ "latest"
        , "stake-address"
        , "build"
        , "--mainnet"
        , "--staking-verification-key-file"
        , verificationKeyFile
        ]

    goldenRewardsAddress <- H.readFile goldenRewardAddressFile

    equivalence rewardAddress goldenRewardsAddress
