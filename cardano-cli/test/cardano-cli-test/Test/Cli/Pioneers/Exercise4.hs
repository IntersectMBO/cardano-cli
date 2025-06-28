{-# LANGUAGE OverloadedStrings #-}

module Test.Cli.Pioneers.Exercise4
  ( hprop_createStakeAddressRegistrationCertificate
  )
where

import Control.Monad (void)

import Test.Cardano.CLI.Util
import Test.Cardano.CLI.Workspace

import Hedgehog (Property)
import Hedgehog.Extras.Test.File qualified as H

-- | 1. Generate a stake verification key
--   2. Create a stake address registration certificate
hprop_createStakeAddressRegistrationCertificate :: Property
hprop_createStakeAddressRegistrationCertificate =
  watchdogProp . propertyOnce . moduleWorkspace2 "tmp" $ \tempDir -> do
    -- Key filepaths
    verKey <- noteTempFile tempDir "stake-verification-key-file"
    signKey <- noteTempFile tempDir "stake-signing-key-file"
    stakeRegCert <- noteTempFile tempDir "stake-registration-certificate-file"

    -- Generate stake verification key
    void $
      execCardanoCLI
        [ "latest"
        , "stake-address"
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
        [ "conway"
        , "stake-address"
        , "registration-certificate"
        , "--stake-verification-key-file"
        , verKey
        , "--key-reg-deposit-amt"
        , "2000000"
        , "--out-file"
        , stakeRegCert
        ]

    H.assertFilesExist [verKey, signKey, stakeRegCert]
