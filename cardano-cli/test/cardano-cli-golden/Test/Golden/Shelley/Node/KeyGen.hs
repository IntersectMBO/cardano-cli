{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Node.KeyGen
   where

import           Control.Monad (void)

import           Test.Cardano.CLI.Aeson
import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelleyNodeKeyGen :: Property
hprop_golden_shelleyNodeKeyGen = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
  signingKeyFile <- noteTempFile tempDir "key-gen.skey"
  opCertCounterFile <- noteTempFile tempDir "op-cert.counter"

  void $ execCardanoCLI
    [ "node","key-gen"
    , "--verification-key-file", verificationKeyFile
    , "--signing-key-file", signingKeyFile
    , "--operational-certificate-issue-counter", opCertCounterFile
    ]

  assertHasMappings [("type", "StakePoolVerificationKey_ed25519"), ("description", "Stake Pool Operator Verification Key")] verificationKeyFile
  assertHasMappings [("type", "StakePoolSigningKey_ed25519"), ("description", "Stake Pool Operator Signing Key")] signingKeyFile
  assertHasMappings [("type", "NodeOperationalCertificateIssueCounter"), ("description", "Next certificate issue number: 0")] opCertCounterFile

  H.assertEndsWithSingleNewline verificationKeyFile
  H.assertEndsWithSingleNewline signingKeyFile
  H.assertEndsWithSingleNewline opCertCounterFile

hprop_golden_shelleyNodeKeyGen_te :: Property
hprop_golden_shelleyNodeKeyGen_te = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
  signingKeyFile <- noteTempFile tempDir "key-gen.skey"
  opCertCounterFile <- noteTempFile tempDir "op-cert.counter"

  void $ execCardanoCLI
    [ "node","key-gen"
    , "--verification-key-file", verificationKeyFile
    , "--signing-key-file", signingKeyFile
    , "--operational-certificate-issue-counter", opCertCounterFile
    ]

  assertHasMappings [("type", "StakePoolVerificationKey_ed25519"), ("description", "Stake Pool Operator Verification Key")] verificationKeyFile
  assertHasMappings [("type", "StakePoolSigningKey_ed25519"), ("description", "Stake Pool Operator Signing Key")] signingKeyFile
  assertHasMappings [("type", "NodeOperationalCertificateIssueCounter"), ("description", "Next certificate issue number: 0")] opCertCounterFile

  H.assertEndsWithSingleNewline verificationKeyFile
  H.assertEndsWithSingleNewline signingKeyFile
  H.assertEndsWithSingleNewline opCertCounterFile

hprop_golden_shelleyNodeKeyGen_bech32 :: Property
hprop_golden_shelleyNodeKeyGen_bech32 = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
  signingKeyFile <- noteTempFile tempDir "key-gen.skey"
  opCertCounterFile <- noteTempFile tempDir "op-cert.counter"

  void $ execCardanoCLI
    [ "node","key-gen"
    , "--key-output-format", "bech32"
    , "--verification-key-file", verificationKeyFile
    , "--signing-key-file", signingKeyFile
    , "--operational-certificate-issue-counter", opCertCounterFile
    ]

  H.assertFileOccurences 1 "pool_vk" verificationKeyFile
  H.assertFileOccurences 1 "pool_sk" signingKeyFile
  assertHasMappings [("type", "NodeOperationalCertificateIssueCounter"), ("description", "Next certificate issue number: 0")] opCertCounterFile

  H.assertEndsWithSingleNewline opCertCounterFile
