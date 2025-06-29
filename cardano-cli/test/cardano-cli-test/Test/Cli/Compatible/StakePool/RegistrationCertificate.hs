{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cli.Compatible.StakePool.RegistrationCertificate where

import Cardano.Api.Era
import Cardano.Api.Pretty

import Data.Aeson (Value)
import Data.Char (toLower)

import Test.Cardano.CLI.Hedgehog qualified as H
import Test.Cardano.CLI.Util

import Hedgehog

hprop_compatible_stake_pool_registration_certificate :: Property
hprop_compatible_stake_pool_registration_certificate =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    refOutFile <- H.noteTempFile tempDir "reference_tx.traw"
    outFile <- H.noteTempFile tempDir "tx.traw"
    let eraName = map toLower . docToString $ pretty ConwayEra

    coldVerKey <- noteTempFile tempDir "cold-verification-key-file"
    coldSignKey <- noteTempFile tempDir "cold-signing-key-file"
    operationalCertCounter <- noteTempFile tempDir "operational-certificate-counter-file"
    _ <-
      execCardanoCLI
        [ eraName
        , "node"
        , "key-gen"
        , "--cold-verification-key-file"
        , coldVerKey
        , "--cold-signing-key-file"
        , coldSignKey
        , "--operational-certificate-issue-counter"
        , operationalCertCounter
        ]

    vrfVerKey <- noteTempFile tempDir "vrf-verification-key-file"
    vrfSignKey <- noteTempFile tempDir "vrf-signing-key-file"
    _ <-
      execCardanoCLI
        [ eraName
        , "node"
        , "key-gen-VRF"
        , "--verification-key-file"
        , vrfVerKey
        , "--signing-key-file"
        , vrfSignKey
        ]

    poolRewardAccountAndOwnerVerKey <- noteTempFile tempDir "reward-account-verification-key-file"
    poolRewardAccountSignKey <- noteTempFile tempDir "reward-account-signing-key-file"
    _ <-
      execCardanoCLI
        [ eraName
        , "stake-address"
        , "key-gen"
        , "--verification-key-file"
        , poolRewardAccountAndOwnerVerKey
        , "--signing-key-file"
        , poolRewardAccountSignKey
        ]

    let args =
          [ "--cold-verification-key-file"
          , coldVerKey
          , "--vrf-verification-key-file"
          , vrfVerKey
          , "--mainnet"
          , "--pool-cost"
          , "1000"
          , "--pool-pledge"
          , "5000"
          , "--pool-margin"
          , "0.1"
          , "--pool-reward-account-verification-key-file"
          , poolRewardAccountAndOwnerVerKey
          , "--pool-owner-stake-verification-key-file"
          , poolRewardAccountAndOwnerVerKey
          ]

    _ <-
      execCardanoCLI $
        [ eraName
        , "stake-pool"
        , "registration-certificate"
        ]
          <> args
          <> [ "--out-file"
             , refOutFile
             ]

    _ <-
      execCardanoCLI $
        [ "compatible"
        , eraName
        , "stake-pool"
        , "registration-certificate"
        ]
          <> args
          <> [ "--out-file"
             , outFile
             ]

    refCert <- H.readJsonFileOk @Value refOutFile
    testCert <- H.readJsonFileOk outFile

    refCert === testCert
