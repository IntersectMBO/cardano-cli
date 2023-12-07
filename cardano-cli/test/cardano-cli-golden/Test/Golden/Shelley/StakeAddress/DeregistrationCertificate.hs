{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.StakeAddress.DeregistrationCertificate where

import           Control.Monad (void)
import           System.FilePath ((</>))

import           Test.Cardano.CLI.Util

import           Hedgehog (Property, forAll)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified Hedgehog.Extras.Test.Golden as H
import           Hedgehog.Gen as Gen

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelley_stake_address_deregistration_certificate_shelley_to_babbage :: Property
hprop_golden_shelley_stake_address_deregistration_certificate_shelley_to_babbage = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- The certificate type will be CertificateShelley for every era from Shelley to Babbage
  eraName <- forAll $ Gen.element ["shelley", "allegra", "mary", "alonzo", "babbage"]
  base <- H.getProjectBase

  verificationKeyFile <- noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/stake_keys/verification_key"
  deregistrationCertFile <- noteTempFile tempDir "deregistrationCertFile"
  scriptDeregistrationCertFile <- noteTempFile tempDir "scripDeregistrationCertFile"
  exampleScript <- noteInputFile $ base </> "scripts/plutus/scripts/v1/custom-guess-42-datum-42.plutus"

  void $ execCardanoCLI
    [ eraName, "stake-address","deregistration-certificate"
    , "--staking-verification-key-file", verificationKeyFile
    , "--out-file", deregistrationCertFile
    ]

  goldenFile1 <- H.note "test/cardano-cli-golden/files/golden/shelley/stake-address/dereg-certificate-1.json"
  H.diffFileVsGoldenFile deregistrationCertFile goldenFile1

  void $ execCardanoCLI
    [ eraName, "stake-address","deregistration-certificate"
    , "--stake-script-file", exampleScript
    , "--out-file", scriptDeregistrationCertFile
    ]

  goldenFile2 <- H.note "test/cardano-cli-golden/files/golden/shelley/stake-address/script-dereg-certificate-1.json"
  H.diffFileVsGoldenFile scriptDeregistrationCertFile goldenFile2

hprop_golden_shelley_stake_address_deregistration_certificate_conway :: Property
hprop_golden_shelley_stake_address_deregistration_certificate_conway = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  base <- H.getProjectBase

  verificationKeyFile <- noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/stake_keys/verification_key"
  deregistrationCertFile <- noteTempFile tempDir "deregistrationCertFile"
  scriptDeregistrationCertFile <- noteTempFile tempDir "scripDeregistrationCertFile"
  exampleScript <- noteInputFile $ base </> "scripts/plutus/scripts/v1/custom-guess-42-datum-42.plutus"

  void $ execCardanoCLI
    [ "conway", "stake-address","deregistration-certificate"
    , "--staking-verification-key-file", verificationKeyFile
    , "--key-reg-deposit-amt", "2000000"
    , "--out-file", deregistrationCertFile
    ]

  goldenFile1 <- H.note "test/cardano-cli-golden/files/golden/shelley/stake-address/dereg-certificate-2.json"
  H.diffFileVsGoldenFile deregistrationCertFile goldenFile1

  void $ execCardanoCLI
    [ "conway", "stake-address","deregistration-certificate"
    , "--stake-script-file", exampleScript
    , "--key-reg-deposit-amt", "2000000"
    , "--out-file", scriptDeregistrationCertFile
    ]

  goldenFile2 <- H.note "test/cardano-cli-golden/files/golden/shelley/stake-address/script-dereg-certificate-2.json"
  H.diffFileVsGoldenFile scriptDeregistrationCertFile goldenFile2
