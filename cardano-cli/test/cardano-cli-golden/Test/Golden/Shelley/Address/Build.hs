{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Address.Build where

import           Control.Monad (void)

import           Test.Cardano.CLI.Util as OP

import           Hedgehog (Property)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelleyAddressBuild :: Property
hprop_golden_shelleyAddressBuild = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  addressVKeyFile <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/payment_keys/verification_key"
  addressSKeyFile <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/stake_keys/verification_key"
  goldenStakingAddressHexFile <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/addresses/staking-address.hex"
  goldenEnterpriseAddressHexFile <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/addresses/enterprise-address.hex"
  stakingAddressHexFile <- noteTempFile tempDir "staking-address.hex"
  enterpriseAddressHexFile <- noteTempFile tempDir "enterprise-address.hex"

  void $ H.readFile addressVKeyFile

  stakingAddressText <-
    execCardanoCLI
      [ "address"
      , "build"
      , "--testnet-magic"
      , "14"
      , "--payment-verification-key-file"
      , addressVKeyFile
      , "--staking-verification-key-file"
      , addressSKeyFile
      ]

  goldenStakingAddressHex <- H.readFile goldenStakingAddressHexFile

  H.writeFile stakingAddressHexFile stakingAddressText

  equivalence stakingAddressText goldenStakingAddressHex

  void $ H.readFile addressSKeyFile

  enterpriseAddressText <-
    execCardanoCLI
      [ "address"
      , "build"
      , "--testnet-magic"
      , "14"
      , "--payment-verification-key-file"
      , addressVKeyFile
      , "--staking-verification-key-file"
      , addressSKeyFile
      ]

  goldenEnterpriseAddressHex <- H.readFile goldenEnterpriseAddressHexFile

  H.writeFile enterpriseAddressHexFile enterpriseAddressText

  equivalence enterpriseAddressText goldenEnterpriseAddressHex
