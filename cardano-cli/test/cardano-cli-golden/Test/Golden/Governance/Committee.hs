{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use camelCase" -}

module Test.Golden.Governance.Committee where

import           Control.Monad (void)
import           Text.Regex.TDFA ((=~))

import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

hprop_golden_governanceCommitteeKeyGenCold :: Property
hprop_golden_governanceCommitteeKeyGenCold =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
    signingKeyFile <- noteTempFile tempDir "key-gen.skey"

    void $ execCardanoCLI
      [ "conway", "governance", "committee", "key-gen-cold"
      , "--verification-key-file", verificationKeyFile
      , "--signing-key-file", signingKeyFile
      ]

    H.assertFileOccurences 1 "ConstitutionalCommitteeColdVerificationKey_ed25519" verificationKeyFile
    H.assertFileOccurences 1 "ConstitutionalCommitteeColdSigningKey_ed25519" signingKeyFile

    H.assertEndsWithSingleNewline verificationKeyFile
    H.assertEndsWithSingleNewline signingKeyFile

hprop_golden_governanceCommitteeKeyGenHot :: Property
hprop_golden_governanceCommitteeKeyGenHot =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
    signingKeyFile <- noteTempFile tempDir "key-gen.skey"

    void $ execCardanoCLI
      [  "conway", "governance", "committee", "key-gen-hot"
      , "--verification-key-file", verificationKeyFile
      , "--signing-key-file", signingKeyFile
      ]

    H.assertFileOccurences 1 "ConstitutionalCommitteeHotVerificationKey_ed25519" verificationKeyFile
    H.assertFileOccurences 1 "ConstitutionalCommitteeHotSigningKey_ed25519" signingKeyFile

    H.assertFileOccurences 1 "Constitutional Committee Hot Verification Key" verificationKeyFile
    H.assertFileOccurences 1 "Constitutional Committee Hot Signing Key" signingKeyFile

    H.assertEndsWithSingleNewline verificationKeyFile
    H.assertEndsWithSingleNewline signingKeyFile

hprop_golden_governanceCommitteeKeyHashCold :: Property
hprop_golden_governanceCommitteeKeyHashCold =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
    signingKeyFile <- noteTempFile tempDir "key-gen.skey"

    void $ execCardanoCLI
      [ "conway", "governance", "committee", "key-gen-cold"
      , "--verification-key-file", verificationKeyFile
      , "--signing-key-file", signingKeyFile
      ]

    result <- execCardanoCLI
      [  "conway", "governance", "committee", "key-hash"
      , "--verification-key-file", verificationKeyFile
      ]

    H.assert $ result =~ id @String "^[a-f0-9]{56}$"

hprop_golden_governanceCommitteeKeyHashHot :: Property
hprop_golden_governanceCommitteeKeyHashHot =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
    signingKeyFile <- noteTempFile tempDir "key-gen.skey"

    void $ execCardanoCLI
      [  "conway", "governance", "committee", "key-gen-hot"
      , "--verification-key-file", verificationKeyFile
      , "--signing-key-file", signingKeyFile
      ]

    result <- execCardanoCLI
      [  "conway", "governance", "committee", "key-hash"
      , "--verification-key-file", verificationKeyFile
      ]

    H.assert $ result =~ id @String "^[a-f0-9]{56}$"
