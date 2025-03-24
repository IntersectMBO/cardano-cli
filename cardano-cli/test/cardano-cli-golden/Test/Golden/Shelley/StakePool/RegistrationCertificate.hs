{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.StakePool.RegistrationCertificate where

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.Golden qualified as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelley_stake_pool_registration_certificate :: Property
hprop_golden_shelley_stake_pool_registration_certificate = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  operatorVerificationKeyFile <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/node-pool/operator.vkey"
  vrfVerificationKeyFile <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/node-pool/vrf.vkey"
  ownerVerificationKeyFile <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/node-pool/owner.vkey"
  registrationCertFile <- noteTempFile tempDir "registration.cert"

  void $
    execCardanoCLI
      [ "conway"
      , "stake-pool"
      , "registration-certificate"
      , "--testnet-magic"
      , "42"
      , "--pool-pledge"
      , "0"
      , "--pool-cost"
      , "0"
      , "--pool-margin"
      , "0"
      , "--cold-verification-key-file"
      , operatorVerificationKeyFile
      , "--vrf-verification-key-file"
      , vrfVerificationKeyFile
      , "--reward-account-verification-key-file"
      , ownerVerificationKeyFile
      , "--pool-owner-stake-verification-key-file"
      , ownerVerificationKeyFile
      , "--out-file"
      , registrationCertFile
      ]

  goldenFile <- H.note "test/cardano-cli-golden/files/golden/shelley/reg-certificate.json"

  H.diffFileVsGoldenFile registrationCertFile goldenFile

hprop_golden_conway_stake_pool_registration_certificate_extended_cold_key :: Property
hprop_golden_conway_stake_pool_registration_certificate_extended_cold_key = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  operatorVerificationKeyFile <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/node-pool/extended-operator.vkey"
  vrfVerificationKeyFile <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/node-pool/vrf.vkey"
  ownerVerificationKeyFile <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/node-pool/owner.vkey"
  registrationCertFile <- noteTempFile tempDir "registration.cert"

  void $
    execCardanoCLI
      [ "conway"
      , "stake-pool"
      , "registration-certificate"
      , "--testnet-magic"
      , "42"
      , "--pool-pledge"
      , "0"
      , "--pool-cost"
      , "0"
      , "--pool-margin"
      , "0"
      , "--cold-verification-key-file"
      , operatorVerificationKeyFile
      , "--vrf-verification-key-file"
      , vrfVerificationKeyFile
      , "--pool-reward-account-verification-key-file"
      , ownerVerificationKeyFile
      , "--pool-owner-stake-verification-key-file"
      , ownerVerificationKeyFile
      , "--out-file"
      , registrationCertFile
      ]

  goldenFile <- H.note "test/cardano-cli-golden/files/golden/shelley/reg-certificate-extended.json"

  H.diffFileVsGoldenFile registrationCertFile goldenFile
