{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.StakeAddress.RegistrationCertificate where

import Control.Monad (void)
import System.FilePath ((</>))

import Test.Cardano.CLI.Util

import Hedgehog
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.Golden qualified as H
import Hedgehog.Extras.Test.Process qualified as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelley_stake_address_registration_certificate :: Property
hprop_golden_shelley_stake_address_registration_certificate =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    base <- H.getProjectBase

    keyGenStakingVerificationKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/stake_keys/verification_key"
    registrationCertFile <- noteTempFile tempDir "registration.cert"
    scriptRegistrationCertFile <- noteTempFile tempDir "script-registration.cert"
    exampleScript <-
      noteInputFile $ base </> "scripts/plutus/scripts/v1/custom-guess-42-datum-42.plutus"

    void $
      execCardanoCLI
        [ "conway"
        , "stake-address"
        , "registration-certificate"
        , "--staking-verification-key-file"
        , keyGenStakingVerificationKeyFile
        , "--key-reg-deposit-amt"
        , "2000000"
        , "--out-file"
        , registrationCertFile
        ]

    goldenFile1 <-
      H.note "test/cardano-cli-golden/files/golden/shelley/stake-address/reg-certificate-1.json"
    H.diffFileVsGoldenFile registrationCertFile goldenFile1

    void $
      execCardanoCLI
        [ "conway"
        , "stake-address"
        , "registration-certificate"
        , "--stake-script-file"
        , exampleScript
        , "--key-reg-deposit-amt"
        , "2000000"
        , "--out-file"
        , scriptRegistrationCertFile
        ]

    goldenFile2 <-
      H.note "test/cardano-cli-golden/files/golden/shelley/stake-address/script-reg-certificate.json"
    H.diffFileVsGoldenFile scriptRegistrationCertFile goldenFile2

hprop_golden_shelley_stake_address_registration_certificate_with_build_raw :: Property
hprop_golden_shelley_stake_address_registration_certificate_with_build_raw =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    keyGenStakingVerificationKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/stake_keys/verification_key"
    registrationCertFile <- noteTempFile tempDir "registration.cert"
    txRawFile <- noteTempFile tempDir "tx.raw"

    void $
      execCardanoCLI
        [ "conway"
        , "stake-address"
        , "registration-certificate"
        , "--staking-verification-key-file"
        , keyGenStakingVerificationKeyFile
        , "--key-reg-deposit-amt"
        , "2000000"
        , "--out-file"
        , registrationCertFile
        ]

    goldenFile1 <-
      H.note "test/cardano-cli-golden/files/golden/shelley/stake-address/reg-certificate-2.json"
    H.diffFileVsGoldenFile registrationCertFile goldenFile1

    void $
      execCardanoCLI
        [ "conway"
        , "transaction"
        , "build-raw"
        , "--tx-in"
        , "bdfa7d91a29ffe071c028c0143c5d278c0a7ddb829c1e95f54a1676915fd82c2#0"
        , "--fee"
        , "1"
        , "--certificate-file"
        , registrationCertFile
        , "--out-file"
        , txRawFile
        ]

    goldenFile2 <-
      H.note "test/cardano-cli-golden/files/golden/shelley/stake-address/build-raw-out.json"
    H.diffFileVsGoldenFile txRawFile goldenFile2

hprop_golden_shelley_stake_address_registration_certificate_missing_reg_deposit :: Property
hprop_golden_shelley_stake_address_registration_certificate_missing_reg_deposit =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    keyGenStakingVerificationKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/stake_keys/verification_key"
    registrationCertFile <- noteTempFile tempDir "registration.cert"

    void $
      execDetailCardanoCLI
        [ "conway"
        , "stake-address"
        , "registration-certificate"
        , "--staking-verification-key-file"
        , keyGenStakingVerificationKeyFile
        , -- , "--key-reg-deposit-amt", "2000000" This argument being mandatory in conway, the call should fail
          "--out-file"
        , registrationCertFile
        ]
