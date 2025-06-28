{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cli.Compatible.StakeAddress.DelegationCertificate where

import Cardano.Api.Era
import Cardano.Api.Pretty

import Control.Monad
import Data.Aeson (Value)
import Data.Char (toLower)

import Test.Cardano.CLI.Util

import Hedgehog
import Hedgehog.Extras qualified as H

hprop_compatible_stake_address_delegation_certificate :: Property
hprop_compatible_stake_address_delegation_certificate =
  propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
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
          , "--stake-pool-id"
          , "ff7b882facd434ac990c4293aa60f3b8a8016e7ad51644939597e90c"
          ]

    void $
      execCardanoCLI $
        [ eraName
        , "stake-address"
        , "stake-delegation-certificate"
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
        , "stake-delegation-certificate"
        ]
          <> args
          <> [ "--out-file"
             , outFile
             ]

    refCert <- H.readJsonFileOk @Value refOutFile
    testCert <- H.readJsonFileOk outFile

    refCert === testCert
