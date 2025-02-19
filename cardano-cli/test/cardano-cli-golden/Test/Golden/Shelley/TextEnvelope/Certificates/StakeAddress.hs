{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.TextEnvelope.Certificates.StakeAddress where

import Cardano.Api

import Control.Monad (void)
import Data.Char (toLower)

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.File qualified as H

{- HLINT ignore "Use camelCase" -}

-- | 1. Generate a stake verification key
--   2. Create a stake address registration certificate
--   3. Check the TextEnvelope serialization format has not changed.
hprop_golden_shelleyStakeAddressCertificates :: Property
hprop_golden_shelleyStakeAddressCertificates = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  let era = BabbageEra
      eraStr = map toLower . docToString $ pretty era

  -- Reference files
  referenceRegistrationCertificate <-
    noteInputFile
      "test/cardano-cli-golden/files/input/shelley/certificates/stake_address_registration_certificate"
  referenceDeregistrationCertificate <-
    noteInputFile
      "test/cardano-cli-golden/files/input/shelley/certificates/stake_address_deregistration_certificate"
  referenceDelegationCertificate <-
    noteInputFile
      "test/cardano-cli-golden/files/input/shelley/certificates/stake_address_delegation_certificate"
  operatorVkey <- noteInputFile "test/cardano-cli-golden/files/input/shelley/node-pool/operator.vkey"

  -- Key filepaths
  verKey <- noteTempFile tempDir "stake-verification-key-file"
  signKey <- noteTempFile tempDir "stake-signing-key-file"
  deregistrationCertificate <- noteTempFile tempDir "stake-address-deregistration-certificate"
  registrationCertificate <- noteTempFile tempDir "stake-address-registration-certificate"

  -- Generate stake verification key
  void $
    execCardanoCLI
      [ eraStr
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
      [ eraStr
      , "stake-address"
      , "registration-certificate"
      , "--stake-verification-key-file"
      , verKey
      , "--out-file"
      , registrationCertificate
      ]

  let registrationCertificateType = textEnvelopeTypeInEra era AsCertificate

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat
    registrationCertificateType
    referenceRegistrationCertificate
    registrationCertificate

  -- Create stake address deregistration certificate
  void $
    execCardanoCLI
      [ eraStr
      , "stake-address"
      , "deregistration-certificate"
      , "--stake-verification-key-file"
      , verKey
      , "--out-file"
      , deregistrationCertificate
      ]

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat
    registrationCertificateType
    referenceDeregistrationCertificate
    deregistrationCertificate

  -- Create stake address delegation certificate
  void $
    execCardanoCLI
      [ eraStr
      , "stake-address"
      , "stake-delegation-certificate"
      , "--stake-verification-key-file"
      , verKey
      , "--cold-verification-key-file"
      , operatorVkey
      , "--out-file"
      , deregistrationCertificate
      ]

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat
    registrationCertificateType
    referenceDelegationCertificate
    deregistrationCertificate
