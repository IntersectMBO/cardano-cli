{-# LANGUAGE CPP #-}
{- HLINT ignore "Use camelCase" -}

module Test.Golden.Governance.DRep where

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

import           Control.Monad

#ifdef UNIX
import           Data.Bits ((.&.))
import           GHC.Stack (withFrozenCallStack)
import           Numeric (showOct)
import           System.Posix.Files (fileMode, getFileStatus)
#endif

import           Test.Cardano.CLI.Util (execCardanoCLI, noteInputFile, noteTempFile, propertyOnce)

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Golden as H

hprop_golden_governanceDRepKeyGen :: Property
hprop_golden_governanceDRepKeyGen =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    verificationKeyFile <- H.noteTempFile tempDir "key-gen.vkey"
    signingKeyFile <- H.noteTempFile tempDir "key-gen.skey"

    void $ execCardanoCLI
      [  "conway", "governance", "drep", "key-gen"
      , "--verification-key-file", verificationKeyFile
      , "--signing-key-file", signingKeyFile
      ]

    H.assertFileOccurences 1 "DRepVerificationKey_ed25519" verificationKeyFile
    H.assertFileOccurences 1 "DRepSigningKey_ed25519" signingKeyFile

    H.assertFileOccurences 1 "Delegate Representative Verification Key" verificationKeyFile
    H.assertFileOccurences 1 "Delegate Representative Signing Key" signingKeyFile

#ifdef UNIX
    vrfMode <- retrievePermissions verificationKeyFile
    sgnMode <- retrievePermissions signingKeyFile

    vrfMode === "600"
    sgnMode === "600"
    where
      retrievePermissions path = withFrozenCallStack $ do
        mode <- H.evalIO $ fileMode <$> getFileStatus path
        pure $ showOct (mode .&. 0o777) "" -- we only need the 3 lowest octets here
#endif

hprop_golden_governance_drep_id_bech32 :: Property
hprop_golden_governance_drep_id_bech32 =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    vkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/drep.vkey"
    idFile <- H.noteTempFile tempDir "drep.id.bech32"
    idGold <- H.note "test/cardano-cli-golden/files/golden/governance/drep/drep.id.bech32"

    void $ execCardanoCLI
      [  "conway", "governance", "drep", "id"
      , "--drep-verification-key-file", vkeyFile
      , "--output-format", "bech32"
      , "--out-file", idFile
      ]

    H.diffFileVsGoldenFile idFile idGold

hprop_golden_governance_drep_id_hex :: Property
hprop_golden_governance_drep_id_hex =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    vkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/drep.vkey"
    idFile <- H.noteTempFile tempDir "drep.id.hex"
    idGold <- H.note "test/cardano-cli-golden/files/golden/governance/drep/drep.id.hex"

    void $ execCardanoCLI
      [  "conway", "governance", "drep", "id"
      , "--drep-verification-key-file", vkeyFile
      , "--output-format", "hex"
      , "--out-file", idFile
      ]

    H.diffFileVsGoldenFile idFile idGold

hprop_golden_governance_drep_retirement_certificate_vkey_file :: Property
hprop_golden_governance_drep_retirement_certificate_vkey_file =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    drepVKeyFile <- noteInputFile "test/cardano-cli-golden/files/input/drep.vkey"
    certFile <- H.noteTempFile tempDir "drep.retirement.cert"
    goldenDRepRetirementCertFile <- H.note "test/cardano-cli-golden/files/golden/governance/drep/drep_retirement_cert"

    void $ execCardanoCLI
      [  "conway", "governance", "drep", "retirement-certificate"
      , "--drep-verification-key-file", drepVKeyFile
      , "--deposit-amt", "1000000"
      , "--out-file", certFile
      ]

    H.diffFileVsGoldenFile certFile goldenDRepRetirementCertFile

hprop_golden_governance_drep_retirement_certificate_id_hex :: Property
hprop_golden_governance_drep_retirement_certificate_id_hex =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    certFile <- H.noteTempFile tempDir "drep.retirement.cert"
    goldenDRepRetirementCertFile <- H.note "test/cardano-cli-golden/files/golden/governance/drep/drep_retirement_cert"

    idFile <- H.readFile "test/cardano-cli-golden/files/input/drep.id.hex"

    void $ execCardanoCLI
      [  "conway", "governance", "drep", "retirement-certificate"
      , "--drep-key-hash", idFile
      , "--deposit-amt", "1000000"
      , "--out-file", certFile
      ]

    H.diffFileVsGoldenFile certFile goldenDRepRetirementCertFile

hprop_golden_governance_drep_retirement_certificate_id_bech32 :: Property
hprop_golden_governance_drep_retirement_certificate_id_bech32 =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    certFile <- H.noteTempFile tempDir "drep.retirement.cert"
    goldenDRepRetirementCertFile <- H.note "test/cardano-cli-golden/files/golden/governance/drep/drep_retirement_cert"

    idFile <- H.readFile "test/cardano-cli-golden/files/input/drep.id.bech32"

    void $ execCardanoCLI
      [  "conway", "governance", "drep", "retirement-certificate"
      , "--drep-key-hash", idFile
      , "--deposit-amt", "1000000"
      , "--out-file", certFile
      ]

    H.diffFileVsGoldenFile certFile goldenDRepRetirementCertFile

hprop_golden_governance_drep_metadata_hash :: Property
hprop_golden_governance_drep_metadata_hash = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  goldenDRepMetadataHash <- H.note "test/cardano-cli-golden/files/golden/governance/drep/drep_metadata_hash"

  drepMetadataFile <- noteTempFile tempDir "drep-metadata.json"
  H.evalIO $ writeFile drepMetadataFile "{ \"Lorem\": \"ipsum\", \"dolor\": \"sit\", \"amet\": \"consectetur\" }"

  outputDRepMetadataHash <- H.noteTempFile tempDir "drep-metadata-hash.txt"

  void $ execCardanoCLI
    [ "conway", "governance", "drep","metadata-hash"
    , "--drep-metadata-file", drepMetadataFile
    , "--out-file", outputDRepMetadataHash
    ]

  H.diffFileVsGoldenFile outputDRepMetadataHash goldenDRepMetadataHash

hprop_golden_governance_drep_registration_certificate_vkey_file :: Property
hprop_golden_governance_drep_registration_certificate_vkey_file = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  drepVKeyFile <- noteInputFile "test/cardano-cli-golden/files/input/drep.vkey"
  goldenFile <- H.note "test/cardano-cli-golden/files/golden/governance/drep/drep_registration_certificate.json"

  outFile <- H.noteTempFile tempDir "drep-reg-cert.txt"

  void $ execCardanoCLI
    [ "conway", "governance", "drep", "registration-certificate"
    , "--drep-verification-key-file", drepVKeyFile
    , "--key-reg-deposit-amt", "0"
    , "--drep-metadata-url", "dummy-url"
    , "--drep-metadata-hash", "52e69500a92d80f2126c836a4903dc582006709f004cf7a28ed648f732dff8d2"
    , "--out-file", outFile
    ]

  H.diffFileVsGoldenFile outFile goldenFile

hprop_golden_governance_drep_registration_certificate_id_hex :: Property
hprop_golden_governance_drep_registration_certificate_id_hex = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  goldenFile <- H.note "test/cardano-cli-golden/files/golden/governance/drep/drep_registration_certificate.json"
  idFile <- H.readFile "test/cardano-cli-golden/files/input/drep.id.hex"

  outFile <- H.noteTempFile tempDir "drep-reg-cert.txt"

  void $ execCardanoCLI
    [ "conway", "governance", "drep", "registration-certificate"
    , "--drep-key-hash", idFile
    , "--key-reg-deposit-amt", "0"
    , "--drep-metadata-url", "dummy-url"
    , "--drep-metadata-hash", "52e69500a92d80f2126c836a4903dc582006709f004cf7a28ed648f732dff8d2"
    , "--out-file", outFile
    ]

  H.diffFileVsGoldenFile outFile goldenFile

hprop_golden_governance_drep_registration_certificate_id_bech32 :: Property
hprop_golden_governance_drep_registration_certificate_id_bech32 = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  goldenFile <- H.note "test/cardano-cli-golden/files/golden/governance/drep/drep_registration_certificate.json"
  idFile <- H.readFile "test/cardano-cli-golden/files/input/drep.id.bech32"

  outFile <- H.noteTempFile tempDir "drep-reg-cert.txt"

  void $ execCardanoCLI
    [ "conway", "governance", "drep", "registration-certificate"
    , "--drep-key-hash", idFile
    , "--key-reg-deposit-amt", "0"
    , "--drep-metadata-url", "dummy-url"
    , "--drep-metadata-hash", "52e69500a92d80f2126c836a4903dc582006709f004cf7a28ed648f732dff8d2"
    , "--out-file", outFile
    ]

  H.diffFileVsGoldenFile outFile goldenFile
