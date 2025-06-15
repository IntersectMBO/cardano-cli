{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Node.KeyGen where

import Control.Monad (void)

import Test.Cardano.CLI.Aeson
import Test.Cardano.CLI.Util

import Hedgehog.Extras (UnitIO)
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.File qualified as H

{- HLINT ignore "Use camelCase" -}

tasty_golden_shelleyNodeKeyGen :: UnitIO ()
tasty_golden_shelleyNodeKeyGen =
  H.moduleWorkspace "tmp" $ \tempDir -> do
    verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
    signingKeyFile <- noteTempFile tempDir "key-gen.skey"
    opCertCounterFile <- noteTempFile tempDir "op-cert.counter"

    void $
      execCardanoCLI
        [ "latest"
        , "node"
        , "key-gen"
        , "--verification-key-file"
        , verificationKeyFile
        , "--signing-key-file"
        , signingKeyFile
        , "--operational-certificate-issue-counter"
        , opCertCounterFile
        ]

    assertHasMappings
      [ ("type", "StakePoolVerificationKey_ed25519")
      , ("description", "Stake Pool Operator Verification Key")
      ]
      verificationKeyFile
    assertHasMappings
      [("type", "StakePoolSigningKey_ed25519"), ("description", "Stake Pool Operator Signing Key")]
      signingKeyFile
    assertHasMappings
      [ ("type", "NodeOperationalCertificateIssueCounter")
      , ("description", "Next certificate issue number: 0")
      ]
      opCertCounterFile

    H.assertEndsWithSingleNewline verificationKeyFile
    H.assertEndsWithSingleNewline signingKeyFile
    H.assertEndsWithSingleNewline opCertCounterFile

tasty_golden_shelleyNodeKeyGen_te :: UnitIO ()
tasty_golden_shelleyNodeKeyGen_te =
  H.moduleWorkspace "tmp" $ \tempDir -> do
    verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
    signingKeyFile <- noteTempFile tempDir "key-gen.skey"
    opCertCounterFile <- noteTempFile tempDir "op-cert.counter"

    void $
      execCardanoCLI
        [ "latest"
        , "node"
        , "key-gen"
        , "--verification-key-file"
        , verificationKeyFile
        , "--signing-key-file"
        , signingKeyFile
        , "--operational-certificate-issue-counter"
        , opCertCounterFile
        ]

    assertHasMappings
      [ ("type", "StakePoolVerificationKey_ed25519")
      , ("description", "Stake Pool Operator Verification Key")
      ]
      verificationKeyFile
    assertHasMappings
      [("type", "StakePoolSigningKey_ed25519"), ("description", "Stake Pool Operator Signing Key")]
      signingKeyFile
    assertHasMappings
      [ ("type", "NodeOperationalCertificateIssueCounter")
      , ("description", "Next certificate issue number: 0")
      ]
      opCertCounterFile

    H.assertEndsWithSingleNewline verificationKeyFile
    H.assertEndsWithSingleNewline signingKeyFile
    H.assertEndsWithSingleNewline opCertCounterFile

tasty_golden_shelleyNodeKeyGen_bech32 :: UnitIO ()
tasty_golden_shelleyNodeKeyGen_bech32 =
  H.moduleWorkspace "tmp" $ \tempDir -> do
    verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
    signingKeyFile <- noteTempFile tempDir "key-gen.skey"
    opCertCounterFile <- noteTempFile tempDir "op-cert.counter"

    void $
      execCardanoCLI
        [ "latest"
        , "node"
        , "key-gen"
        , "--key-output-format"
        , "bech32"
        , "--verification-key-file"
        , verificationKeyFile
        , "--signing-key-file"
        , signingKeyFile
        , "--operational-certificate-issue-counter"
        , opCertCounterFile
        ]

    H.assertFileOccurences 1 "pool_vk" verificationKeyFile
    H.assertFileOccurences 1 "pool_sk" signingKeyFile
    assertHasMappings
      [ ("type", "NodeOperationalCertificateIssueCounter")
      , ("description", "Next certificate issue number: 0")
      ]
      opCertCounterFile

    H.assertEndsWithSingleNewline opCertCounterFile
