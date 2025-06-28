{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.StakeAddress.Build where

import Test.Cardano.CLI.Util as OP

import Hedgehog.Extras (UnitIO)
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.File qualified as H

{- HLINT ignore "Use camelCase" -}

tasty_golden_shelleyStakeAddressBuild :: UnitIO ()
tasty_golden_shelleyStakeAddressBuild =
  H.moduleWorkspace "tmp" $ \_ -> do
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
