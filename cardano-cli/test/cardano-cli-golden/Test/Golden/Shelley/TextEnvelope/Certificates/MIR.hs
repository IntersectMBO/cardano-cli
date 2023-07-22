{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.TextEnvelope.Certificates.MIR
  ( golden_shelleyMIRCertificate
  ) where

import           Cardano.Api (AsType (..), CardanoEra (..), textEnvelopeTypeInEra)

import           Control.Monad (void)

import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

-- | 1. Generate stake key pair
--   2. Create MIR certificate
--   s. Check the TextEnvelope serialization format has not changed.
golden_shelleyMIRCertificate :: Property
golden_shelleyMIRCertificate = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  let era = BabbageEra

  -- Reference keys
  referenceMIRCertificate <- noteInputFile "test/cardano-cli-golden/files/golden/shelley/certificates/mir_certificate"

  -- Key filepaths
  verKey <- noteTempFile tempDir "stake-verification-key-file"
  signKey <- noteTempFile tempDir "stake-signing-key-file"
  mirCertificate <- noteTempFile tempDir "mir-certificate-file"

  -- Generate stake key pair
  void $ execCardanoCLI
    [ "stake-address","key-gen"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    ]

  H.assertFilesExist [verKey, signKey]

  let testAddr = "stake1u9j6axhcpd0exvrthn5dqzqt54g85akqvkn4uqmccm70qsc5hpv9w"
  -- Create MIR certificate
  void $ execCardanoCLI
    [ "babbage", "governance", "create-mir-certificate"
    , "--reserves" --TODO: Should also do "--reserves"
    , "--stake-address", testAddr
    , "--reward", "1000"
    , "--out-file", mirCertificate
    ]

  H.assertFilesExist [mirCertificate]

  let registrationCertificateType = textEnvelopeTypeInEra era AsCertificate

  checkTextEnvelopeFormat registrationCertificateType referenceMIRCertificate mirCertificate
