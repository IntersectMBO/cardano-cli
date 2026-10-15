{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Address.Build where

import Control.Monad (void)

import Test.Cardano.CLI.Util as OP

import Hedgehog (Property)
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.File qualified as H
import Hedgehog.Extras.Test.Golden qualified as H

hprop_golden_shelleyAddressBuild :: Property
hprop_golden_shelleyAddressBuild =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
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
        [ "latest"
        , "address"
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
        [ "latest"
        , "address"
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

-- Regression tests for https://github.com/IntersectMBO/cardano-cli/issues/1448
hprop_golden_addressBuildPlutusV4PaymentScript :: Property
hprop_golden_addressBuildPlutusV4PaymentScript =
  watchdogProp . propertyOnce $ do
    address <-
      execCardanoCLI
        [ "latest"
        , "address"
        , "build"
        , "--testnet-magic"
        , "14"
        , "--payment-script-file"
        , plutusV4ScriptFile
        ]

    H.diffVsGoldenFile
      address
      "test/cardano-cli-golden/files/golden/dijkstra/address/build-payment-script-v4.addr"

hprop_golden_addressBuildPlutusV4StakeScript :: Property
hprop_golden_addressBuildPlutusV4StakeScript =
  watchdogProp . propertyOnce $ do
    address <-
      execCardanoCLI
        [ "latest"
        , "address"
        , "build"
        , "--testnet-magic"
        , "14"
        , "--payment-verification-key-file"
        , "test/cardano-cli-golden/files/input/shelley/keys/payment_keys/verification_key"
        , "--stake-script-file"
        , plutusV4ScriptFile
        ]

    H.diffVsGoldenFile
      address
      "test/cardano-cli-golden/files/golden/dijkstra/address/build-stake-script-v4.addr"

plutusV4ScriptFile :: FilePath
plutusV4ScriptFile = "test/cardano-cli-golden/files/input/dijkstra/plutus/v4-always-succeeds.plutus"
