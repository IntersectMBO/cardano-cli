{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.TextEnvelope.Certificates.GenesisKeyDelegation where

import Cardano.Api
  ( AsType (..)
  , CardanoEra (..)
  , cardanoEraConstraints
  , textEnvelopeTypeInEra
  )

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog.Extras (UnitIO)
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.File qualified as H

{- HLINT ignore "Use camelCase" -}

tasty_golden_shelleyGenesisKeyDelegationCertificate :: UnitIO ()
tasty_golden_shelleyGenesisKeyDelegationCertificate =
  H.moduleWorkspace "tmp" $ \tempDir -> do
    let era = BabbageEra

    -- Reference certificate
    referenceCertificateFilePath <-
      noteInputFile $
        "test/cardano-cli-golden/files/input/shelley/certificates/"
          <> "genesis_key_delegation_certificate"

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

    let certificateType = cardanoEraConstraints era $ textEnvelopeTypeInEra era AsCertificate

    checkTextEnvelopeFormat
      certificateType
      referenceCertificateFilePath
      genesisKeyDelegCertFilePath
