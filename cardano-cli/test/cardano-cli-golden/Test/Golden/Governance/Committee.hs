{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use camelCase" -}

module Test.Golden.Governance.Committee where

import Control.Monad (void)
import Text.Regex.TDFA ((=~))

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog qualified as H
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.File qualified as H

hprop_golden_governanceCommitteeKeyGenCold :: Property
hprop_golden_governanceCommitteeKeyGenCold =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
    signingKeyFile <- noteTempFile tempDir "key-gen.skey"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "committee"
        , "key-gen-cold"
        , "--verification-key-file"
        , verificationKeyFile
        , "--signing-key-file"
        , signingKeyFile
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

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "committee"
        , "key-gen-hot"
        , "--verification-key-file"
        , verificationKeyFile
        , "--signing-key-file"
        , signingKeyFile
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

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "committee"
        , "key-gen-cold"
        , "--verification-key-file"
        , verificationKeyFile
        , "--signing-key-file"
        , signingKeyFile
        ]

    result <-
      execCardanoCLI
        [ "conway"
        , "governance"
        , "committee"
        , "key-hash"
        , "--verification-key-file"
        , verificationKeyFile
        ]

    H.assert $ result =~ id @String "^[a-f0-9]{56}$"

hprop_golden_governanceCommitteeKeyHashHot :: Property
hprop_golden_governanceCommitteeKeyHashHot =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
    signingKeyFile <- noteTempFile tempDir "key-gen.skey"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "committee"
        , "key-gen-hot"
        , "--verification-key-file"
        , verificationKeyFile
        , "--signing-key-file"
        , signingKeyFile
        ]

    result <-
      execCardanoCLI
        [ "conway"
        , "governance"
        , "committee"
        , "key-hash"
        , "--verification-key-file"
        , verificationKeyFile
        ]

    H.assert $ result =~ id @String "^[a-f0-9]{56}$"

hprop_golden_governanceCommitteeCreateHotKeyAuthorizationCertificate :: Property
hprop_golden_governanceCommitteeCreateHotKeyAuthorizationCertificate =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    ccColdVKey <- noteTempFile tempDir "cc-cold.vkey"
    ccColdSKey <- noteTempFile tempDir "cc-cold.skey"
    ccHotVKey <- noteTempFile tempDir "cc-hot.vkey"
    ccHotSKey <- noteTempFile tempDir "cc-hot.skey"

    certFile <- noteTempFile tempDir "hot-auth.cert"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "committee"
        , "key-gen-cold"
        , "--verification-key-file"
        , ccColdVKey
        , "--signing-key-file"
        , ccColdSKey
        ]

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "committee"
        , "key-gen-hot"
        , "--verification-key-file"
        , ccHotVKey
        , "--signing-key-file"
        , ccHotSKey
        ]

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "committee"
        , "create-hot-key-authorization-certificate"
        , "--cold-verification-key-file"
        , ccColdVKey
        , "--hot-key-file"
        , ccHotVKey
        , "--out-file"
        , certFile
        ]

    H.assertFileOccurences 1 "CertificateShelley" certFile
    H.assertFileOccurences 1 "Constitutional Committee Hot Key Registration Certificate" certFile

hprop_golden_governanceCommitteeCreateColdKeyResignationCertificate :: Property
hprop_golden_governanceCommitteeCreateColdKeyResignationCertificate =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    ccColdVKey <- noteTempFile tempDir "cold.vkey"
    ccColdSKey <- noteTempFile tempDir "cold.skey"

    certFile <- noteTempFile tempDir "hot-auth.cert"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "committee"
        , "key-gen-cold"
        , "--verification-key-file"
        , ccColdVKey
        , "--signing-key-file"
        , ccColdSKey
        ]

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "committee"
        , "create-cold-key-resignation-certificate"
        , "--cold-verification-key-file"
        , ccColdVKey
        , "--out-file"
        , certFile
        ]

    H.assertFileOccurences 1 "CertificateShelley" certFile
    H.assertFileOccurences 1 "Constitutional Committee Cold Key Resignation Certificate" certFile
