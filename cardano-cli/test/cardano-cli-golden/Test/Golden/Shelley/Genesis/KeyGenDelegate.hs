{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Genesis.KeyGenDelegate where

import Control.Monad (void)

import Test.Cardano.CLI.Aeson
import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.File qualified as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelley_genesis_key_gen_delegate :: Property
hprop_golden_shelley_genesis_key_gen_delegate =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
    signingKeyFile <- noteTempFile tempDir "key-gen.skey"
    operationalCertificateIssueCounterFile <- noteTempFile tempDir "op-cert.counter"

    void $
      execCardanoCLI
        [ "latest"
        , "genesis"
        , "key-gen-delegate"
        , "--verification-key-file"
        , verificationKeyFile
        , "--signing-key-file"
        , signingKeyFile
        , "--operational-certificate-issue-counter"
        , operationalCertificateIssueCounterFile
        ]

    assertHasMappings
      [ ("type", "GenesisDelegateVerificationKey_ed25519")
      , ("description", "Genesis delegate operator key")
      ]
      verificationKeyFile
    assertHasKeys ["cborHex"] verificationKeyFile
    H.assertEndsWithSingleNewline verificationKeyFile

    assertHasMappings
      [ ("type", "GenesisDelegateSigningKey_ed25519")
      , ("description", "Genesis delegate operator key")
      ]
      signingKeyFile
    assertHasKeys ["cborHex"] signingKeyFile
    H.assertEndsWithSingleNewline signingKeyFile

    assertHasMappings
      [ ("type", "NodeOperationalCertificateIssueCounter")
      , ("description", "Next certificate issue number: 0")
      ]
      operationalCertificateIssueCounterFile
    assertHasKeys ["cborHex"] operationalCertificateIssueCounterFile
    H.assertEndsWithSingleNewline operationalCertificateIssueCounterFile
