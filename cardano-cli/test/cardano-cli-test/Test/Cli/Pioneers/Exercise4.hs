{-# LANGUAGE OverloadedStrings #-}

module Test.Cli.Pioneers.Exercise4
  ( hprop_createStakeAddressRegistrationCertificate
  ) where

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

-- | 1. Generate a stake verification key
--   2. Create a stake address registration certificate
hprop_createStakeAddressRegistrationCertificate :: Property
hprop_createStakeAddressRegistrationCertificate = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Key filepaths
  verKey <- noteTempFile tempDir "stake-verification-key-file"
  signKey <- noteTempFile tempDir "stake-signing-key-file"
  stakeRegCert <- noteTempFile tempDir "stake-registration-certificate-file"

  -- Generate stake verification key
  void $
    execCardanoCLI
      [ "stake-address"
      , "key-gen"
      , "--verification-key-file"
      , verKey
      , "--signing-key-file"
      , signKey
      ]
  H.assertFilesExist [verKey, signKey]

  -- Create stake address registration certificate
  void $
    execCardanoCLI
      [ "babbage"
      , "stake-address"
      , "registration-certificate"
      , "--stake-verification-key-file"
      , verKey
      , "--out-file"
      , stakeRegCert
      ]

  H.assertFilesExist [verKey, signKey, stakeRegCert]
