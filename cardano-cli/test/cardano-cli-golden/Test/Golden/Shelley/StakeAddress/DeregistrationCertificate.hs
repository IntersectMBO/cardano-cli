{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.StakeAddress.DeregistrationCertificate where

import           Test.Cardano.CLI.Polysemy

import           HaskellWorks.Polysemy.Hedgehog
import           HaskellWorks.Polysemy.Hedgehog.Golden
import           Polysemy ()

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelley_stake_address_deregistration_certificate :: Property
hprop_golden_shelley_stake_address_deregistration_certificate = propertyOnce $ localWorkspace $ do
  verificationKeyFile <- jotPkgInputFile "test/cardano-cli-golden/files/input/shelley/keys/stake_keys/verification_key"
  deregistrationCertFile <- jotTempFile "deregistrationCertFile"
  scriptDeregistrationCertFile <- jotTempFile "scripDeregistrationCertFile"
  exampleScript <- jotRootInputFile "scripts/plutus/scripts/v1/custom-guess-42-datum-42.plutus"

  jot_ exampleScript

  execCardanoCli_
    [ "babbage", "stake-address","deregistration-certificate"
    , "--staking-verification-key-file", verificationKeyFile
    , "--out-file", deregistrationCertFile
    ]

  goldenFile1 <- jotPkgGoldenFile "test/cardano-cli-golden/files/golden/shelley/dereg-cert-1.json"
  diffFileVsGoldenFile deregistrationCertFile goldenFile1

  execCardanoCli_
    [ "babbage", "stake-address","deregistration-certificate"
    , "--stake-script-file", exampleScript
    , "--out-file", scriptDeregistrationCertFile
    ]

  goldenFile2 <- jotPkgGoldenFile "test/cardano-cli-golden/files/golden/shelley/dereg-cert-2.json"

  diffFileVsGoldenFile scriptDeregistrationCertFile goldenFile2
