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
