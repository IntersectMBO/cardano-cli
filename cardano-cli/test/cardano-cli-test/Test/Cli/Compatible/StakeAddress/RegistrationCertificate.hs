{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cli.Compatible.StakeAddress.RegistrationCertificate where

import Cardano.Api.Internal.Eras
import Cardano.Api.Internal.Pretty

import Control.Monad
import Data.Aeson (Value)
import Data.Char (toLower)

import Test.Cardano.CLI.Util

import Hedgehog
import Hedgehog.Extras qualified as H

hprop_compatible_stake_address_registration_certificate :: Property
hprop_compatible_stake_address_registration_certificate =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    refOutFile <- H.noteTempFile tempDir "stake-registration-certificate.reference.json"
    outFile <- H.noteTempFile tempDir "stake-registration-certificate.json"
    let eraName = map toLower . docToString $ pretty ConwayEra

    verKey <- noteTempFile tempDir "stake-verification-key-file"
    signKey <- noteTempFile tempDir "stake-signing-key-file"

    void $
      execCardanoCLI
        [ eraName
        , "stake-address"
        , "key-gen"
        , "--verification-key-file"
        , verKey
        , "--signing-key-file"
        , signKey
        ]

    H.assertFilesExist [verKey, signKey]

    let args =
          [ "--stake-verification-key-file"
          , verKey
          , "--key-reg-deposit-amt"
          , "100000"
          ]

    void $
      execCardanoCLI $
        [ eraName
        , "stake-address"
        , "registration-certificate"
        ]
          <> args
          <> [ "--out-file"
             , refOutFile
             ]

    void $
      execCardanoCLI $
        [ "compatible"
        , eraName
        , "stake-address"
        , "registration-certificate"
        ]
          <> args
          <> [ "--out-file"
             , outFile
             ]

    refCert <- H.readJsonFileOk @Value refOutFile
    testCert <- H.readJsonFileOk outFile

    refCert === testCert
