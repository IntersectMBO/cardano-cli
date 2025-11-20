{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cli.Certificates.GenesisKeyDelegationCertificate where

import Cardano.Api (File (..), ShelleyLedgerEra, readFileTextEnvelope)
import Cardano.Api.Experimental as Exp

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog (Property, evalIO, success)
import Hedgehog.Extras qualified as H
import Hedgehog.Internal.Property (failWith)

hprop_golden_shelleyGenesisKeyDelegationCertificate :: Property
hprop_golden_shelleyGenesisKeyDelegationCertificate =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- Verification key and certificate filepaths
    genesisVerKeyFilePath <-
      noteTempFile tempDir "genesis-verification-key-file"
    genesisDelegVerKeyFilePath <-
      noteTempFile tempDir "genesis-delegate-verification-key-file"
    vrfVerKeyFilePath <- noteTempFile tempDir "vrf-verification-key-file"
    genesisKeyDelegCertFilePath <-
      noteTempFile tempDir "genesis-key-delegation-certificate-file"

    -- Signing Key filepaths
    genesisSignKeyFilePath <- noteTempFile tempDir "genesis-signing-key-file"
    genesisDelegSignKeyFilePath <- noteTempFile tempDir "genesis-delegate-signing-key-file"
    vrfSignKeyFilePath <- noteTempFile tempDir "vrf-signing-key-file"

    genesisDelegOpCertCounterFilePath <- noteTempFile tempDir "genesis-delegate-opcert-counter"

    -- Generate genesis key pair
    void $
      execCardanoCLI
        [ "latest"
        , "genesis"
        , "key-gen-genesis"
        , "--verification-key-file"
        , genesisVerKeyFilePath
        , "--signing-key-file"
        , genesisSignKeyFilePath
        ]

    -- Generate genesis delegate key pair
    void $
      execCardanoCLI
        [ "latest"
        , "genesis"
        , "key-gen-delegate"
        , "--verification-key-file"
        , genesisDelegVerKeyFilePath
        , "--signing-key-file"
        , genesisDelegSignKeyFilePath
        , "--operational-certificate-issue-counter-file"
        , genesisDelegOpCertCounterFilePath
        ]

    -- Generate VRF key pair
    void $
      execCardanoCLI
        [ "latest"
        , "node"
        , "key-gen-VRF"
        , "--verification-key-file"
        , vrfVerKeyFilePath
        , "--signing-key-file"
        , vrfSignKeyFilePath
        ]

    H.assertFilesExist
      [ genesisVerKeyFilePath
      , genesisDelegVerKeyFilePath
      , vrfVerKeyFilePath
      ]

    -- Create genesis key delegation certificate
    void $
      execCardanoCLI
        [ "compatible"
        , "babbage"
        , "governance"
        , "create-genesis-key-delegation-certificate"
        , "--genesis-verification-key-file"
        , genesisVerKeyFilePath
        , "--genesis-delegate-verification-key-file"
        , genesisDelegVerKeyFilePath
        , "--vrf-verification-key-file"
        , vrfVerKeyFilePath
        , "--out-file"
        , genesisKeyDelegCertFilePath
        ]

    H.assertFilesExist [genesisKeyDelegCertFilePath]
    eCert <-
      evalIO $ readFileTextEnvelope $ File genesisKeyDelegCertFilePath
    case eCert of
      Left err -> failWith Nothing $ "Failed to decode generated genesis key delegation certificate: " <> show err
      Right (_cert :: Exp.Certificate (ShelleyLedgerEra BabbageEra)) -> success
