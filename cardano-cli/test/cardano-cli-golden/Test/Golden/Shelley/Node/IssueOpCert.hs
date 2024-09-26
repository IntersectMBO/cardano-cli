{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Node.IssueOpCert where

import           Control.Monad (void)

import           Test.Cardano.CLI.Aeson (assertHasMappings)
import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelleyNodeIssueOpCert :: Property
hprop_golden_shelleyNodeIssueOpCert = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  hotKesVerificationKeyFile <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/kes_keys/verification_key"
  coldSigningKeyFile <-
    noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/genesis_delegate_keys/signing_key"
  originalOperationalCertificateIssueCounterFile <-
    noteInputFile
      "test/cardano-cli-golden/files/input/shelley/keys/genesis_delegate_keys/operational_certificate_counter"
  operationalCertificateIssueCounterFile <- noteTempFile tempDir "delegate-op-cert.counter"
  operationalCertFile <- noteTempFile tempDir "operational.cert"

  H.copyFile originalOperationalCertificateIssueCounterFile operationalCertificateIssueCounterFile

  -- We could generate the required keys here, but then if the KES generation fails this
  -- test would also fail which is misleading.
  -- However, the keys can be generated eg:
  --    cabal run cardano-cli:cardano-cli -- shelley node key-gen-KES \
  --        --verification-key-file cardano-cli/test/cli/node-issue-op-cert/data/node-kes.vkey \
  --        --signing-key-file /dev/null
  void $
    execCardanoCLI
      [ "latest"
      , "node"
      , "issue-op-cert"
      , "--hot-kes-verification-key-file"
      , hotKesVerificationKeyFile
      , "--cold-signing-key-file"
      , coldSigningKeyFile
      , "--operational-certificate-issue-counter"
      , operationalCertificateIssueCounterFile
      , "--kes-period"
      , "0"
      , "--out-file"
      , operationalCertFile
      ]

  assertHasMappings [("type", "NodeOperationalCertificate")] operationalCertFile
  assertHasMappings
    [ ("type", "NodeOperationalCertificateIssueCounter")
    , ("description", "Next certificate issue number: 1")
    ]
    operationalCertificateIssueCounterFile

  H.assertEndsWithSingleNewline operationalCertFile
  H.assertEndsWithSingleNewline operationalCertificateIssueCounterFile
