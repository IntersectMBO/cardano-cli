{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.StakeAddress.RegistrationCertificate where

import Control.Monad (void)
import System.FilePath ((</>))

import Test.Cardano.CLI.Util

import Hedgehog
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelleyStakeAddressRegistrationCertificate :: Property
hprop_golden_shelleyStakeAddressRegistrationCertificate = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  base <- H.getProjectBase

  keyGenStakingVerificationKeyFile <-
    noteInputFile "test/cardano-cli-golden/files/golden/shelley/keys/stake_keys/verification_key"
  registrationCertFile <- noteTempFile tempDir "registration.cert"
  scriptRegistrationCertFile <- noteTempFile tempDir "script-registration.cert"
  exampleScript <-
    noteInputFile $ base </> "scripts/plutus/scripts/v1/custom-guess-42-datum-42.plutus"

  void $
    execCardanoCLI
      [ "babbage"
      , "stake-address"
      , "registration-certificate"
      , "--staking-verification-key-file"
      , keyGenStakingVerificationKeyFile
      , "--out-file"
      , registrationCertFile
      ]

  H.assertFileOccurences 1 "Stake Address Registration Certificate" registrationCertFile

  void $
    execCardanoCLI
      [ "babbage"
      , "stake-address"
      , "registration-certificate"
      , "--stake-script-file"
      , exampleScript
      , "--out-file"
      , scriptRegistrationCertFile
      ]

  H.assertFileOccurences 1 "Stake Address Registration Certificate" scriptRegistrationCertFile

  H.assertEndsWithSingleNewline registrationCertFile

hprop_golden_shelleyStakeAddressRegistrationCertificateWithBuildRaw :: Property
hprop_golden_shelleyStakeAddressRegistrationCertificateWithBuildRaw = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  keyGenStakingVerificationKeyFile <-
    noteInputFile "test/cardano-cli-golden/files/golden/shelley/keys/stake_keys/verification_key"
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

  H.assertFileOccurences 1 "Stake Address Registration Certificate" registrationCertFile

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
