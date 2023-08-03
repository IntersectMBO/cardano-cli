{- HLINT ignore "Use camelCase" -}

module Test.Golden.Governance.Committee
  ( governanceCommitteeTests
  ) where

import           Control.Monad (void)

import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

governanceCommitteeTests :: IO Bool
governanceCommitteeTests =
  H.checkSequential $ H.Group "Governance Committee Goldens"
    [ ("governance committee key-gen-cold", golden_governanceCommitteeKeyGenCold)
    , ("governance committee key-gen-hot", golden_governanceCommitteeKeyGenHot)
    ]

golden_governanceCommitteeKeyGenCold :: Property
golden_governanceCommitteeKeyGenCold =
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

golden_governanceCommitteeKeyGenHot :: Property
golden_governanceCommitteeKeyGenHot =
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
