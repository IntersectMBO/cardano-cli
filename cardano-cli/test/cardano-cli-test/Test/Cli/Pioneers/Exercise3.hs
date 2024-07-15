{-# LANGUAGE OverloadedStrings #-}

module Test.Cli.Pioneers.Exercise3
  ( hprop_createOperationalCertificate
  )
where

import           Control.Monad (void)

import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

-- | 1. Create KES key pair.
--   2. Create cold keys.
--   3. Create operational certificate.
--   4. Create VRF key pair.
hprop_createOperationalCertificate :: Property
hprop_createOperationalCertificate = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Key filepaths
  kesVerKey <- noteTempFile tempDir "KES-verification-key-file"
  kesSignKey <- noteTempFile tempDir "KES-signing-key-file"
  coldVerKey <- noteTempFile tempDir "cold-verification-key-file"
  coldSignKey <- noteTempFile tempDir "cold-signing-key-file"
  operationalCertCounter <- noteTempFile tempDir "operational-certificate-counter-file"
  operationalCert <- noteTempFile tempDir "operational-certificate-file"

  -- Create KES key pair
  void $
    execCardanoCLI
      [ "node"
      , "key-gen-KES"
      , "--verification-key-file"
      , kesVerKey
      , "--signing-key-file"
      , kesSignKey
      ]

  H.assertFilesExist [kesSignKey, kesVerKey]

  -- Create cold key pair
  void $
    execCardanoCLI
      [ "node"
      , "key-gen"
      , "--cold-verification-key-file"
      , coldVerKey
      , "--cold-signing-key-file"
      , coldSignKey
      , "--operational-certificate-issue-counter"
      , operationalCertCounter
      ]

  H.assertFilesExist [coldVerKey, coldSignKey, operationalCertCounter]

  -- Create operational certificate
  void $
    execCardanoCLI
      [ "node"
      , "issue-op-cert"
      , "--kes-verification-key-file"
      , kesVerKey
      , "--cold-signing-key-file"
      , coldSignKey
      , "--operational-certificate-issue-counter"
      , operationalCertCounter
      , "--kes-period"
      , "1000"
      , "--out-file"
      , operationalCert
      ]

  H.assertFilesExist
    [kesVerKey, kesSignKey, coldVerKey, coldSignKey, operationalCertCounter, operationalCert]
