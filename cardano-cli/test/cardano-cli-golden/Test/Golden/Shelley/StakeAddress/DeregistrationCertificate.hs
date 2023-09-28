{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.StakeAddress.DeregistrationCertificate where

import Control.Monad (void)
import System.FilePath ((</>))

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelleyStakeAddressDeregistrationCertificate :: Property
hprop_golden_shelleyStakeAddressDeregistrationCertificate = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  base <- H.getProjectBase

  verificationKeyFile <-
    noteInputFile "test/cardano-cli-golden/files/golden/shelley/keys/stake_keys/verification_key"
  deregistrationCertFile <- noteTempFile tempDir "deregistrationCertFile"
  scriptDeregistrationCertFile <- noteTempFile tempDir "scripDeregistrationCertFile"
  exampleScript <-
    noteInputFile $ base </> "scripts/plutus/scripts/v1/custom-guess-42-datum-42.plutus"

  void $
    execCardanoCLI
      [ "babbage"
      , "stake-address"
      , "deregistration-certificate"
      , "--staking-verification-key-file"
      , verificationKeyFile
      , "--out-file"
      , deregistrationCertFile
      ]

  H.assertFileOccurences 1 "Stake Address Deregistration Certificate" deregistrationCertFile

  void $
    execCardanoCLI
      [ "babbage"
      , "stake-address"
      , "deregistration-certificate"
      , "--stake-script-file"
      , exampleScript
      , "--out-file"
      , scriptDeregistrationCertFile
      ]

  H.assertFileOccurences 1 "Stake Address Deregistration Certificate" scriptDeregistrationCertFile

  H.assertEndsWithSingleNewline deregistrationCertFile
