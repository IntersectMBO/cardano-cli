{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.StakeAddress.DeregistrationCertificate where

import Control.Monad (void)
import System.FilePath ((</>))

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.Golden qualified as H
import Hedgehog.Extras.Test.Process qualified as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelley_stake_address_deregistration_certificate :: Property
hprop_golden_shelley_stake_address_deregistration_certificate = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  base <- H.getProjectBase

  verificationKeyFile <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/stake_keys/verification_key"
  deregistrationCertFile <- noteTempFile tempDir "deregistrationCertFile"
  scriptDeregistrationCertFile <- noteTempFile tempDir "scripDeregistrationCertFile"
  exampleScript <-
    noteInputFile $ base </> "scripts/plutus/scripts/v1/custom-guess-42-datum-42.plutus"

  void $
    execCardanoCLI
      [ "conway"
      , "stake-address"
      , "deregistration-certificate"
      , "--staking-verification-key-file"
      , verificationKeyFile
      , "--key-reg-deposit-amt"
      , "2000000"
      , "--out-file"
      , deregistrationCertFile
      ]

  goldenFile1 <- H.note "test/cardano-cli-golden/files/golden/shelley/dereg-cert-1.json"
  H.diffFileVsGoldenFile deregistrationCertFile goldenFile1

  void $
    execCardanoCLI
      [ "conway"
      , "stake-address"
      , "deregistration-certificate"
      , "--stake-script-file"
      , exampleScript
      , "--key-reg-deposit-amt"
      , "2000000"
      , "--out-file"
      , scriptDeregistrationCertFile
      ]

  goldenFile2 <- H.note "test/cardano-cli-golden/files/golden/shelley/dereg-cert-2.json"
  H.diffFileVsGoldenFile scriptDeregistrationCertFile goldenFile2
