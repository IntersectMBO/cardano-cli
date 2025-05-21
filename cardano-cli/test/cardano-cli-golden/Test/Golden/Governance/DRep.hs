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

import           GHC.IO.Exception (ExitCode (ExitFailure))
import           Test.Cardano.CLI.Hash (exampleAnchorDataHash, exampleAnchorDataIpfsHash,
                   exampleAnchorDataPathGolden, serveFilesWhile, tamperBase16Hash)
import           Test.Cardano.CLI.Util (diffVsGoldenFileExcludeTrace, execCardanoCLI, execDetailCardanoCLI,
                   noteInputFile, noteTempFile, propertyOnce, watchdogProp)

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Golden as H

drepRetirementCertFile :: FilePath
drepRetirementCertFile = "test/cardano-cli-golden/files/golden/governance/drep/drep_retirement_cert"

drepRegistrationCertFile :: FilePath
drepRegistrationCertFile = "test/cardano-cli-golden/files/golden/governance/drep/drep_registration_certificate.json"

hprop_golden_governanceDRepKeyGen :: Property
hprop_golden_governanceDRepKeyGen =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    verificationKeyFile <- H.noteTempFile tempDir "key-gen.vkey"
    signingKeyFile <- H.noteTempFile tempDir "key-gen.skey"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "drep"
        , "key-gen"
        , "--verification-key-file"
        , verificationKeyFile
        , "--signing-key-file"
        , signingKeyFile
        ]

    H.assertFileOccurences 1 "DRepVerificationKey_ed25519" verificationKeyFile
    H.assertFileOccurences 1 "DRepSigningKey_ed25519" signingKeyFile

    H.assertFileOccurences 1 "Delegated Representative Verification Key" verificationKeyFile
    H.assertFileOccurences 1 "Delegated Representative Signing Key" signingKeyFile

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

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance drep id bech32/"'@
hprop_golden_governance_drep_id_bech32 :: Property
hprop_golden_governance_drep_id_bech32 =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    vkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/drep.vkey"
    idFile <- H.noteTempFile tempDir "drep.id.bech32"
    idGold <- H.note "test/cardano-cli-golden/files/golden/governance/drep/drep.id.bech32"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "drep"
        , "id"
        , "--drep-verification-key-file"
        , vkeyFile
        , "--output-bech32"
        , "--out-file"
        , idFile
        ]

    H.diffFileVsGoldenFile idFile idGold

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance drep id hex/"'@
hprop_golden_governance_drep_id_hex :: Property
hprop_golden_governance_drep_id_hex =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    vkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/drep.vkey"
    idFile <- H.noteTempFile tempDir "drep.id.hex"
    idGold <- H.note "test/cardano-cli-golden/files/golden/governance/drep/drep.id.hex"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "drep"
        , "id"
        , "--drep-verification-key-file"
        , vkeyFile
        , "--output-hex"
        , "--out-file"
        , idFile
        ]

    H.diffFileVsGoldenFile idFile idGold

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance drep id hash/"'@
hprop_golden_governance_drep_id_hash :: Property
hprop_golden_governance_drep_id_hash =
  watchdogProp . propertyOnce $ do
    idGold <- H.note "test/cardano-cli-golden/files/golden/governance/drep/drep.id.hash"

    output <-
      execCardanoCLI
        [ "conway"
        , "governance"
        , "drep"
        , "id"
        , "--drep-key-hash"
        , "c1a342f0dfb82b93ca2e6b406bacb04802f7d56a99d8f95a80a8b6c5"
        ]

    H.diffVsGoldenFile output idGold

hprop_golden_governance_drep_extended_key_signing :: Property
hprop_golden_governance_drep_extended_key_signing =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    skeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/governance/drep/extended-key-signing/drep.skey"
    txBody <-
      noteInputFile "test/cardano-cli-golden/files/input/governance/drep/extended-key-signing/tx.body"

    outFile <- H.noteTempFile tempDir "outFile"
    outGold <-
      H.note "test/cardano-cli-golden/files/golden/governance/drep/extended-key-signing/tx.signed"

    H.noteShowM_ $
      execCardanoCLI
        [ "conway"
        , "transaction"
        , "sign"
        , "--tx-body-file"
        , txBody
        , "--signing-key-file"
        , skeyFile
        , "--out-file"
        , outFile
        ]

    H.diffFileVsGoldenFile outFile outGold

hprop_golden_governance_drep_retirement_certificate_vkey_file :: Property
hprop_golden_governance_drep_retirement_certificate_vkey_file =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    drepVKeyFile <- noteInputFile "test/cardano-cli-golden/files/input/drep.vkey"
    certFile <- H.noteTempFile tempDir "drep.retirement.cert"
    H.noteShow_ drepRetirementCertFile

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "drep"
        , "retirement-certificate"
        , "--drep-verification-key-file"
        , drepVKeyFile
        , "--deposit-amt"
        , "1000000"
        , "--out-file"
        , certFile
        ]

    H.diffFileVsGoldenFile certFile drepRetirementCertFile

hprop_golden_governance_drep_retirement_certificate_id_hex :: Property
hprop_golden_governance_drep_retirement_certificate_id_hex =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    certFile <- H.noteTempFile tempDir "drep.retirement.cert"
    H.noteShow_ drepRetirementCertFile

    idFile <- H.readFile "test/cardano-cli-golden/files/input/drep.id.hex"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "drep"
        , "retirement-certificate"
        , "--drep-key-hash"
        , idFile
        , "--deposit-amt"
        , "1000000"
        , "--out-file"
        , certFile
        ]

    H.diffFileVsGoldenFile certFile drepRetirementCertFile

hprop_golden_governance_drep_retirement_certificate_id_bech32 :: Property
hprop_golden_governance_drep_retirement_certificate_id_bech32 =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    certFile <- H.noteTempFile tempDir "drep.retirement.cert"
    H.noteShow_ drepRetirementCertFile

    idFile <- H.readFile "test/cardano-cli-golden/files/input/drep.id.bech32"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "drep"
        , "retirement-certificate"
        , "--drep-key-hash"
        , idFile
        , "--deposit-amt"
        , "1000000"
        , "--out-file"
        , certFile
        ]

    H.diffFileVsGoldenFile certFile drepRetirementCertFile

hprop_golden_governance_drep_metadata_hash :: Property
hprop_golden_governance_drep_metadata_hash =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    goldenDRepMetadataHash <-
      H.note "test/cardano-cli-golden/files/golden/governance/drep/drep_metadata_hash"

    drepMetadataFile <- noteTempFile tempDir "drep-metadata.json"
    H.evalIO $
      writeFile drepMetadataFile "{ \"Lorem\": \"ipsum\", \"dolor\": \"sit\", \"amet\": \"consectetur\" }"

    outputDRepMetadataHash <- H.noteTempFile tempDir "drep-metadata-hash.txt"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "drep"
        , "metadata-hash"
        , "--drep-metadata-file"
        , drepMetadataFile
        , "--out-file"
        , outputDRepMetadataHash
        ]

    H.diffFileVsGoldenFile outputDRepMetadataHash goldenDRepMetadataHash

-- Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance drep metadata hash cip119/"'@
hprop_golden_governance_drep_metadata_hash_cip119 :: Property
hprop_golden_governance_drep_metadata_hash_cip119 =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    goldenDRepMetadataHashCip119 <-
      H.note "test/cardano-cli-golden/files/golden/governance/drep/drep_metadata_hash_cip119"

    -- Use jsonld file from test vector of CIP119 https://github.com/cardano-foundation/CIPs/blob/master/CIP-0119/test-vector.md
    drepMetadataFile <- noteInputFile "test/cardano-cli-golden/files/input/governance/drep/drep.jsonld"

    outputDRepMetadataHashCip119 <- H.noteTempFile tempDir "drep-metadata-hash-cip119.txt"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "drep"
        , "metadata-hash"
        , "--drep-metadata-file"
        , drepMetadataFile
        , "--out-file"
        , outputDRepMetadataHashCip119
        ]

    H.diffFileVsGoldenFile outputDRepMetadataHashCip119 goldenDRepMetadataHashCip119

hprop_golden_governance_drep_registration_certificate_vkey_file :: Property
hprop_golden_governance_drep_registration_certificate_vkey_file =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    drepVKeyFile <- noteInputFile "test/cardano-cli-golden/files/input/drep.vkey"
    H.noteShow_ drepRegistrationCertFile

    outFile <- H.noteTempFile tempDir "drep-reg-cert.txt"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "drep"
        , "registration-certificate"
        , "--drep-verification-key-file"
        , drepVKeyFile
        , "--key-reg-deposit-amt"
        , "0"
        , "--drep-metadata-url"
        , "dummy-url"
        , "--drep-metadata-hash"
        , "52e69500a92d80f2126c836a4903dc582006709f004cf7a28ed648f732dff8d2"
        , "--out-file"
        , outFile
        ]

    H.diffFileVsGoldenFile outFile drepRegistrationCertFile

hprop_golden_governance_drep_registration_certificate_id_hex :: Property
hprop_golden_governance_drep_registration_certificate_id_hex =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    idFile <- H.readFile "test/cardano-cli-golden/files/input/drep.id.hex"
    H.noteShow_ drepRegistrationCertFile

    outFile <- H.noteTempFile tempDir "drep-reg-cert.txt"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "drep"
        , "registration-certificate"
        , "--drep-key-hash"
        , idFile
        , "--key-reg-deposit-amt"
        , "0"
        , "--drep-metadata-url"
        , "dummy-url"
        , "--drep-metadata-hash"
        , "52e69500a92d80f2126c836a4903dc582006709f004cf7a28ed648f732dff8d2"
        , "--out-file"
        , outFile
        ]

    H.diffFileVsGoldenFile outFile drepRegistrationCertFile

hprop_golden_governance_drep_registration_certificate_id_bech32 :: Property
hprop_golden_governance_drep_registration_certificate_id_bech32 =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    idFile <- H.readFile "test/cardano-cli-golden/files/input/drep.id.bech32"
    H.noteShow_ drepRegistrationCertFile

    outFile <- H.noteTempFile tempDir "drep-reg-cert.txt"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "drep"
        , "registration-certificate"
        , "--drep-key-hash"
        , idFile
        , "--key-reg-deposit-amt"
        , "0"
        , "--drep-metadata-url"
        , "dummy-url"
        , "--drep-metadata-hash"
        , "52e69500a92d80f2126c836a4903dc582006709f004cf7a28ed648f732dff8d2"
        , "--out-file"
        , outFile
        ]

    H.diffFileVsGoldenFile outFile drepRegistrationCertFile

hprop_golden_governance_drep_registration_certificate_script_hash :: Property
hprop_golden_governance_drep_registration_certificate_script_hash =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    goldenFile <-
      H.note
        "test/cardano-cli-golden/files/golden/governance/drep/drep_registration_certificate_script.json"

    outFile <- H.noteTempFile tempDir "drep-reg-cert.txt"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "drep"
        , "registration-certificate"
        , "--drep-script-hash"
        , "00000000000000000000000000000000000000000000000000000003"
        , "--key-reg-deposit-amt"
        , "0"
        , "--drep-metadata-url"
        , "dummy-url"
        , "--drep-metadata-hash"
        , "52e69500a92d80f2126c836a4903dc582006709f004cf7a28ed648f732dff8d2"
        , "--out-file"
        , outFile
        ]

    H.diffFileVsGoldenFile outFile goldenFile

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance drep update certificate vkey file/"'@
hprop_golden_governance_drep_update_certificate_vkey_file :: Property
hprop_golden_governance_drep_update_certificate_vkey_file =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    drepVKeyFile <- noteInputFile "test/cardano-cli-golden/files/input/drep.vkey"
    goldenFile <-
      H.note "test/cardano-cli-golden/files/golden/governance/drep/drep_update_certificate.json"

    outFile <- H.noteTempFile tempDir "drep-upd-cert.txt"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "drep"
        , "update-certificate"
        , "--drep-verification-key-file"
        , drepVKeyFile
        , "--drep-metadata-url"
        , "dummy-url"
        , "--drep-metadata-hash"
        , "52e69500a92d80f2126c836a4903dc582006709f004cf7a28ed648f732dff8d2"
        , "--out-file"
        , outFile
        ]

    H.diffFileVsGoldenFile outFile goldenFile

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance drep update certificate script hash/"'@
hprop_golden_governance_drep_update_certificate_script_hash :: Property
hprop_golden_governance_drep_update_certificate_script_hash =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    goldenFile <-
      H.note
        "test/cardano-cli-golden/files/golden/governance/drep/drep_update_certificate_script_hash.json"
    outFile <- H.noteTempFile tempDir "drep-upd-cert.txt"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "drep"
        , "update-certificate"
        , "--drep-script-hash"
        , "8f33600845940d65bdbc7ea7a247a7997aa8558649128fa82c4c0468"
        , "--drep-metadata-url"
        , "https://raw.githubusercontent.com/cardano-foundation/CIPs/master/CIP-0119/examples/drep.jsonld"
        , "--drep-metadata-hash"
        , "fecc1773db89b45557d82e07719c275f6877a6cadfd2469f4dc5a7df5b38b4a4"
        , "--out-file"
        , outFile
        ]

    H.diffFileVsGoldenFile outFile goldenFile

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden verification key drep/"'@
hprop_golden_verification_key_drep :: Property
hprop_golden_verification_key_drep =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    skeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/governance/drep/extended-key-signing/drep.skey"
    vkeyFileOut <- noteTempFile tempDir "drep.extended.vkey"
    goldenFile <- H.note "test/cardano-cli-golden/files/golden/governance/drep/drep-extended.vkey.out"

    H.noteShowM_ $
      execCardanoCLI
        [ "conway"
        , "key"
        , "verification-key"
        , "--signing-key-file"
        , skeyFile
        , "--verification-key-file"
        , vkeyFileOut
        ]

    H.diffFileVsGoldenFile vkeyFileOut goldenFile

-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/drep metadata hash url wrong hash fails/"'@
hprop_golden_drep_metadata_hash_url_wrong_hash_fails :: Property
hprop_golden_drep_metadata_hash_url_wrong_hash_fails =
  watchdogProp . propertyOnce $ do
    -- We modify the hash slightly so that the hash check fails
    alteredHash <- H.evalMaybe $ tamperBase16Hash exampleAnchorDataHash
    let relativeUrl = [exampleAnchorDataIpfsHash]

    -- Create temporary HTTP server with files required by the call to `cardano-cli`
    (exitCode, _, result) <-
      serveFilesWhile
        [ (relativeUrl, exampleAnchorDataPathGolden)
        ]
        ( \port -> do
            execDetailCardanoCLI
              [ "conway"
              , "governance"
              , "drep"
              , "metadata-hash"
              , "--drep-metadata-url"
              , "http://127.0.0.1:" ++ show port ++ "/" ++ exampleAnchorDataIpfsHash
              , "--expected-hash"
              , alteredHash
              ]
        )

    exitCode === ExitFailure 1

    diffVsGoldenFileExcludeTrace
      result
      "test/cardano-cli-golden/files/golden/governance/drep/drep_metadata_hash_url_wrong_hash_fails.out"
