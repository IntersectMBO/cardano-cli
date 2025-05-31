{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use camelCase" -}

module Test.Golden.Governance.DRep where

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

import           Control.Monad

#ifdef UNIX
import           Data.Bits ((.&.))
import           Numeric (showOct)
import           System.Posix.Files (fileMode, getFileStatus)
#endif

import           Effectful ()
import           Effectful.Zoo.FileSystem
import           Effectful.Zoo.Hedgehog
import           Effectful.Zoo.Hedgehog.Effect.Run
import           Effectful.Zoo.Hedgehog.Golden
import           GHC.IO.Exception (ExitCode (ExitFailure))
import           HaskellWorks.Prelude
import           Test.Cardano.CLI.Effectful.File
import           Test.Cardano.CLI.Effectful.Process
import           Test.Cardano.CLI.Effectful.Workspace
import           Test.Cardano.CLI.Hash (exampleAnchorDataHash, exampleAnchorDataIpfsHash,
                   exampleAnchorDataPathGolden, serveFilesWhile, tamperBase16Hash)

import qualified Hedgehog as H

drepRetirementCertFile :: FilePath
drepRetirementCertFile = "test/cardano-cli-golden/files/golden/governance/drep/drep_retirement_cert"

drepRegistrationCertFile :: FilePath
drepRegistrationCertFile = "test/cardano-cli-golden/files/golden/governance/drep/drep_registration_certificate.json"

tasty_golden_governanceDRepKeyGen :: TestT IO ()
tasty_golden_governanceDRepKeyGen =
  unit . localWorkspace "tmp" $ do
    verificationKeyFile <- jotTempFile "key-gen.vkey"
    signingKeyFile <- jotTempFile "key-gen.skey"

    void $
      execCardanoCliOk
        [ "conway"
        , "governance"
        , "drep"
        , "key-gen"
        , "--verification-key-file"
        , verificationKeyFile
        , "--signing-key-file"
        , signingKeyFile
        ]

    assertFileOccurences 1 "DRepVerificationKey_ed25519" verificationKeyFile
    assertFileOccurences 1 "DRepSigningKey_ed25519" signingKeyFile

    assertFileOccurences 1 "Delegated Representative Verification Key" verificationKeyFile
    assertFileOccurences 1 "Delegated Representative Signing Key" signingKeyFile

#ifdef UNIX
    vrfMode <- retrievePermissions verificationKeyFile
    sgnMode <- retrievePermissions signingKeyFile

    vrfMode === "600"
    sgnMode === "600"
 where
  retrievePermissions path = withFrozenCallStack $ do
    mode <- evalIO $ fileMode <$> getFileStatus path
    pure $ showOct (mode .&. 0o777) "" -- we only need the 3 lowest octets here
#endif

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance drep id bech32/"'@
tasty_golden_governance_drep_id_bech32 :: TestT IO ()
tasty_golden_governance_drep_id_bech32 =
  unit . localWorkspace "tmp" $ do
    vkeyFile <- jotPkgInputFile "test/cardano-cli-golden/files/input/drep.vkey"
    idFile <- jotTempFile "drep.id.bech32"
    idGold <- jotString "test/cardano-cli-golden/files/golden/governance/drep/drep.id.bech32"

    void $
      execCardanoCliOk
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

    diffFileVsGoldenFile idFile idGold

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance drep id hex/"'@
tasty_golden_governance_drep_id_hex :: TestT IO ()
tasty_golden_governance_drep_id_hex =
  unit . localWorkspace "tmp" $ do
    vkeyFile <- jotPkgInputFile "test/cardano-cli-golden/files/input/drep.vkey"
    idFile <- jotTempFile "drep.id.hex"
    idGold <- jotString "test/cardano-cli-golden/files/golden/governance/drep/drep.id.hex"

    void $
      execCardanoCliOk
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

    diffFileVsGoldenFile idFile idGold

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance drep id hash/"'@
tasty_golden_governance_drep_id_hash :: TestT IO ()
tasty_golden_governance_drep_id_hash =
  unit $ do
    idGold <- jotString "test/cardano-cli-golden/files/golden/governance/drep/drep.id.hash"

    output <-
      execCardanoCliOk
        [ "conway"
        , "governance"
        , "drep"
        , "id"
        , "--drep-key-hash"
        , "c1a342f0dfb82b93ca2e6b406bacb04802f7d56a99d8f95a80a8b6c5"
        ]

    diffVsGoldenFile output idGold
      & trapFail @IOException

tasty_golden_governance_drep_extended_key_signing :: TestT IO ()
tasty_golden_governance_drep_extended_key_signing =
  unit . localWorkspace "tmp" $ do
    skeyFile <- jotPkgInputFile "test/cardano-cli-golden/files/input/governance/drep/extended-key-signing/drep.skey"
    txBody <- jotPkgInputFile "test/cardano-cli-golden/files/input/governance/drep/extended-key-signing/tx.body"

    outFile <- jotTempFile "outFile"
    outGold <- jotString "test/cardano-cli-golden/files/golden/governance/drep/extended-key-signing/tx.signed"

    jotShowM_ $
      execCardanoCliOk
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

    diffFileVsGoldenFile outFile outGold

tasty_golden_governance_drep_retirement_certificate_vkey_file :: TestT IO ()
tasty_golden_governance_drep_retirement_certificate_vkey_file =
  unit . localWorkspace "tmp" $ do
    drepVKeyFile <- jotPkgInputFile "test/cardano-cli-golden/files/input/drep.vkey"
    certFile <- jotTempFile "drep.retirement.cert"
    jotShow_ drepRetirementCertFile

    void $
      execCardanoCliOk
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

    diffFileVsGoldenFile certFile drepRetirementCertFile

tasty_golden_governance_drep_retirement_certificate_id_hex :: TestT IO ()
tasty_golden_governance_drep_retirement_certificate_id_hex =
  unit . localWorkspace "tmp" $ do
    certFile <- jotTempFile "drep.retirement.cert"
    jotShow_ drepRetirementCertFile

    idFile <- readStringFile "test/cardano-cli-golden/files/input/drep.id.hex"
      & trapFail @IOException

    void $
      execCardanoCliOk
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

    diffFileVsGoldenFile certFile drepRetirementCertFile

tasty_golden_governance_drep_retirement_certificate_id_bech32 :: TestT IO ()
tasty_golden_governance_drep_retirement_certificate_id_bech32 =
  unit . localWorkspace "tmp" $ do
    certFile <- jotTempFile "drep.retirement.cert"
    jotShow_ drepRetirementCertFile

    idFile <- readStringFile "test/cardano-cli-golden/files/input/drep.id.bech32"
      & trapFail @IOException

    void $
      execCardanoCliOk
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

    diffFileVsGoldenFile certFile drepRetirementCertFile

tasty_golden_governance_drep_metadata_hash :: TestT IO ()
tasty_golden_governance_drep_metadata_hash =
  unit . localWorkspace "tmp" $ do
    goldenDRepMetadataHash <-
      jotString "test/cardano-cli-golden/files/golden/governance/drep/drep_metadata_hash"

    drepMetadataFile <- jotTempFile "drep-metadata.json"
    evalIO $
      writeFile drepMetadataFile "{ \"Lorem\": \"ipsum\", \"dolor\": \"sit\", \"amet\": \"consectetur\" }"

    outputDRepMetadataHash <- jotTempFile "drep-metadata-hash.txt"

    void $
      execCardanoCliOk
        [ "conway"
        , "governance"
        , "drep"
        , "metadata-hash"
        , "--drep-metadata-file"
        , drepMetadataFile
        , "--out-file"
        , outputDRepMetadataHash
        ]

    diffFileVsGoldenFile outputDRepMetadataHash goldenDRepMetadataHash

-- Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance drep metadata hash cip119/"'@
tasty_golden_governance_drep_metadata_hash_cip119 :: TestT IO ()
tasty_golden_governance_drep_metadata_hash_cip119 =
  unit . localWorkspace "tmp" $ do
    goldenDRepMetadataHashCip119 <-
      jotString "test/cardano-cli-golden/files/golden/governance/drep/drep_metadata_hash_cip119"

    -- Use jsonld file from test vector of CIP119 https://github.com/cardano-foundation/CIPs/blob/master/CIP-0119/test-vector.md
    drepMetadataFile <- jotPkgInputFile "test/cardano-cli-golden/files/input/governance/drep/drep.jsonld"

    outputDRepMetadataHashCip119 <- jotTempFile "drep-metadata-hash-cip119.txt"

    void $
      execCardanoCliOk
        [ "conway"
        , "governance"
        , "drep"
        , "metadata-hash"
        , "--drep-metadata-file"
        , drepMetadataFile
        , "--out-file"
        , outputDRepMetadataHashCip119
        ]

    diffFileVsGoldenFile outputDRepMetadataHashCip119 goldenDRepMetadataHashCip119

tasty_golden_governance_drep_registration_certificate_vkey_file :: TestT IO ()
tasty_golden_governance_drep_registration_certificate_vkey_file =
  unit . localWorkspace "tmp" $ do
    drepVKeyFile <- jotPkgInputFile "test/cardano-cli-golden/files/input/drep.vkey"
    jotShow_ drepRegistrationCertFile

    outFile <- jotTempFile "drep-reg-cert.txt"

    void $
      execCardanoCliOk
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

    diffFileVsGoldenFile outFile drepRegistrationCertFile

tasty_golden_governance_drep_registration_certificate_id_hex :: TestT IO ()
tasty_golden_governance_drep_registration_certificate_id_hex =
  unit . localWorkspace "tmp" $ do
    idFile <- readStringFile "test/cardano-cli-golden/files/input/drep.id.hex"
      & trapFail @IOException
    jotShow_ drepRegistrationCertFile

    outFile <- jotTempFile "drep-reg-cert.txt"

    void $
      execCardanoCliOk
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

    diffFileVsGoldenFile outFile drepRegistrationCertFile

tasty_golden_governance_drep_registration_certificate_id_bech32 :: TestT IO ()
tasty_golden_governance_drep_registration_certificate_id_bech32 =
  unit . localWorkspace "tmp" $ do
    idFile <- readStringFile "test/cardano-cli-golden/files/input/drep.id.bech32"
      & trapFail @IOException
    jotShow_ drepRegistrationCertFile

    outFile <- jotTempFile "drep-reg-cert.txt"

    void $
      execCardanoCliOk
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

    diffFileVsGoldenFile outFile drepRegistrationCertFile

tasty_golden_governance_drep_registration_certificate_script_hash :: TestT IO ()
tasty_golden_governance_drep_registration_certificate_script_hash =
  unit . localWorkspace "tmp" $ do
    goldenFile <-
      jotString "test/cardano-cli-golden/files/golden/governance/drep/drep_registration_certificate_script.json"

    outFile <- jotTempFile "drep-reg-cert.txt"

    void $
      execCardanoCliOk
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

    diffFileVsGoldenFile outFile goldenFile

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance drep update certificate vkey file/"'@
tasty_golden_governance_drep_update_certificate_vkey_file :: TestT IO ()
tasty_golden_governance_drep_update_certificate_vkey_file =
  unit . localWorkspace "tmp" $ do
    drepVKeyFile <- jotPkgInputFile "test/cardano-cli-golden/files/input/drep.vkey"
    goldenFile <-
      jotString "test/cardano-cli-golden/files/golden/governance/drep/drep_update_certificate.json"

    outFile <- jotTempFile "drep-upd-cert.txt"

    void $
      execCardanoCliOk
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

    diffFileVsGoldenFile outFile goldenFile

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance drep update certificate script hash/"'@
tasty_golden_governance_drep_update_certificate_script_hash :: TestT IO ()
tasty_golden_governance_drep_update_certificate_script_hash =
  unit . localWorkspace "tmp" $ do
    goldenFile <- jotString "test/cardano-cli-golden/files/golden/governance/drep/drep_update_certificate_script_hash.json"
    outFile <- jotTempFile "drep-upd-cert.txt"

    void $
      execCardanoCliOk
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

    diffFileVsGoldenFile outFile goldenFile

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden verification key drep/"'@
tasty_golden_verification_key_drep :: TestT IO ()
tasty_golden_verification_key_drep =
  unit . localWorkspace "tmp" $ do
    skeyFile <-
      jotPkgInputFile "test/cardano-cli-golden/files/input/governance/drep/extended-key-signing/drep.skey"
    vkeyFileOut <- jotTempFile "drep.extended.vkey"
    goldenFile <- jotString "test/cardano-cli-golden/files/golden/governance/drep/drep-extended.vkey.out"

    jotShowM_ $
      execCardanoCliOk
        [ "conway"
        , "key"
        , "verification-key"
        , "--signing-key-file"
        , skeyFile
        , "--verification-key-file"
        , vkeyFileOut
        ]

    diffFileVsGoldenFile vkeyFileOut goldenFile

-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/drep metadata hash url wrong hash fails/"'@
tasty_golden_drep_metadata_hash_url_wrong_hash_fails :: TestT IO ()
tasty_golden_drep_metadata_hash_url_wrong_hash_fails =
  unit $ do
    -- We modify the hash slightly so that the hash check fails
    alteredHash <- H.evalMaybe $ tamperBase16Hash exampleAnchorDataHash
    let relativeUrl = [exampleAnchorDataIpfsHash]

    -- Create temporary HTTP server with files required by the call to `cardano-cli`
    (exitCode, _, result) <-
      serveFilesWhile
        [ (relativeUrl, exampleAnchorDataPathGolden)
        ]
        ( \port -> do
            execDetailCardanoCliOk
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
