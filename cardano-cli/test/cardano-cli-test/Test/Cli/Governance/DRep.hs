{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE FlexibleContexts #-}

module Test.Cli.Governance.DRep where

import Cardano.Api (MonadIO)

import Control.Monad
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Control (MonadBaseControl)

import Test.Cardano.CLI.Hash
  ( exampleAnchorDataHash
  , exampleAnchorDataIpfsHash
  , exampleAnchorDataPathTest
  , serveFilesWhile
  , tamperBase16Hash
  )
import Test.Cardano.CLI.Util
  ( execCardanoCLI
  , execCardanoCLIWithEnvVars
  , expectFailure
  , propertyOnce
  )

import Hedgehog
import Hedgehog qualified as H
import Hedgehog.Extras.Test.Base qualified as H

metadataUrls :: [String]
metadataUrls =
  [ "dummy-url"
  , "length-84-here-we-goooooooooooooooooooooooooooooooooooooooooooooooooooooo-dummy-url"
  , "exactly-128-chars-here-we-goooooooooooooooooooooooooooooooooooooooooooooooooooooo-dummy-url-ooooooooooooooooooooooooooooooooooo"
  ]

-- | This is a test of https://github.com/IntersectMBO/cardano-cli/issues/552
-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/governance drep registration certificate script hash/"'@
hprop_governance_drep_registration_certificate_script_hash :: Property
hprop_governance_drep_registration_certificate_script_hash =
  propertyOnce $ forM_ metadataUrls $ \metadataUrl -> do
    H.moduleWorkspace "tmp" $ \tempDir -> do
      outFile <- H.noteTempFile tempDir "drep-reg-cert.txt"

      H.noteShowM_ $
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
          , metadataUrl
          , "--drep-metadata-hash"
          , "52e69500a92d80f2126c836a4903dc582006709f004cf7a28ed648f732dff8d2"
          , "--out-file"
          , outFile
          ]

-- | This is a test of https://github.com/IntersectMBO/cardano-cli/issues/552
-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/governance drep update certificate vkey file/"'@
hprop_governance_drep_update_certificate_vkey_file :: Property
hprop_governance_drep_update_certificate_vkey_file =
  propertyOnce $ forM_ metadataUrls $ \metadataUrl -> do
    H.moduleWorkspace "tmp" $ \tempDir -> do
      drepVKeyFile <- H.noteTempFile tempDir "drep.vkey"
      drepSKeyFile <- H.noteTempFile tempDir "drep.skey"

      H.noteShowM_ $
        execCardanoCLI
          [ "conway"
          , "governance"
          , "drep"
          , "key-gen"
          , "--verification-key-file"
          , drepVKeyFile
          , "--signing-key-file"
          , drepSKeyFile
          ]

      outFile <- H.noteTempFile tempDir "drep-upd-cert.txt"

      H.noteShowM_ $
        execCardanoCLI
          [ "conway"
          , "governance"
          , "drep"
          , "update-certificate"
          , "--drep-verification-key-file"
          , drepVKeyFile
          , "--drep-metadata-url"
          , metadataUrl
          , "--drep-metadata-hash"
          , "52e69500a92d80f2126c836a4903dc582006709f004cf7a28ed648f732dff8d2"
          , "--out-file"
          , outFile
          ]

hprop_golden_governance_drep_registration_certificate_vkey_file_wrong_hash_fails :: Property
hprop_golden_governance_drep_registration_certificate_vkey_file_wrong_hash_fails =
  propertyOnce . expectFailure . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- We modify the hash slightly so that the hash check fails
    alteredHash <- H.evalMaybe $ tamperBase16Hash exampleAnchorDataHash
    -- We run the test with the altered
    base_golden_governance_drep_registration_certificate_vkey_file
      alteredHash
      tempDir

hprop_golden_governance_drep_registration_certificate_vkey_file :: Property
hprop_golden_governance_drep_registration_certificate_vkey_file =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir ->
    base_golden_governance_drep_registration_certificate_vkey_file exampleAnchorDataHash tempDir

base_golden_governance_drep_registration_certificate_vkey_file
  :: (MonadBaseControl IO m, MonadTest m, MonadIO m, MonadCatch m) => String -> FilePath -> m ()
base_golden_governance_drep_registration_certificate_vkey_file hash tempDir = do
  drepVKeyFile <- H.noteTempFile tempDir "drep.vkey"
  drepSKeyFile <- H.noteTempFile tempDir "drep.skey"

  H.noteShowM_ $
    execCardanoCLI
      [ "conway"
      , "governance"
      , "drep"
      , "key-gen"
      , "--verification-key-file"
      , drepVKeyFile
      , "--signing-key-file"
      , drepSKeyFile
      ]

  outFile <- H.noteTempFile tempDir "drep-reg-cert.txt"

  let relativeUrl = ["ipfs", exampleAnchorDataIpfsHash]

  -- Create temporary HTTP server with files required by the call to `cardano-cli`
  -- In this case, the server emulates an IPFS gateway
  serveFilesWhile
    [(relativeUrl, exampleAnchorDataPathTest)]
    ( \port -> do
        void $
          execCardanoCLIWithEnvVars
            [("IPFS_GATEWAY_URI", "http://localhost:" ++ show port ++ "/")]
            [ "conway"
            , "governance"
            , "drep"
            , "registration-certificate"
            , "--drep-verification-key-file"
            , drepVKeyFile
            , "--key-reg-deposit-amt"
            , "0"
            , "--drep-metadata-url"
            , "ipfs://" ++ exampleAnchorDataIpfsHash
            , "--drep-metadata-hash"
            , hash
            , "--check-drep-metadata-hash"
            , "--out-file"
            , outFile
            ]
    )

hprop_golden_governance_drep_update_certificate_vkey_file_wrong_hash_fails :: Property
hprop_golden_governance_drep_update_certificate_vkey_file_wrong_hash_fails =
  propertyOnce . expectFailure . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- We modify the hash slightly so that the hash check fails
    alteredHash <- H.evalMaybe $ tamperBase16Hash exampleAnchorDataHash
    -- We run the test with the modified hash
    base_golden_governance_drep_update_certificate_vkey_file
      alteredHash
      tempDir

hprop_golden_governance_drep_update_certificate_vkey_file :: Property
hprop_golden_governance_drep_update_certificate_vkey_file =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir ->
    base_golden_governance_drep_update_certificate_vkey_file exampleAnchorDataHash tempDir

base_golden_governance_drep_update_certificate_vkey_file
  :: (MonadBaseControl IO m, MonadTest m, MonadIO m, MonadCatch m) => String -> FilePath -> m ()
base_golden_governance_drep_update_certificate_vkey_file hash tempDir = do
  drepVKeyFile <- H.noteTempFile tempDir "drep.vkey"
  drepSKeyFile <- H.noteTempFile tempDir "drep.skey"

  H.noteShowM_ $
    execCardanoCLI
      [ "conway"
      , "governance"
      , "drep"
      , "key-gen"
      , "--verification-key-file"
      , drepVKeyFile
      , "--signing-key-file"
      , drepSKeyFile
      ]

  outFile <- H.noteTempFile tempDir "drep-upd-cert.txt"

  let relativeUrl = ["ipfs", exampleAnchorDataIpfsHash]

  -- Create temporary HTTP server with files required by the call to `cardano-cli`
  -- In this case, the server emulates an IPFS gateway
  serveFilesWhile
    [(relativeUrl, exampleAnchorDataPathTest)]
    ( \port -> do
        void $
          execCardanoCLIWithEnvVars
            [("IPFS_GATEWAY_URI", "http://localhost:" ++ show port ++ "/")]
            [ "conway"
            , "governance"
            , "drep"
            , "update-certificate"
            , "--drep-verification-key-file"
            , drepVKeyFile
            , "--drep-metadata-url"
            , "ipfs://" ++ exampleAnchorDataIpfsHash
            , "--drep-metadata-hash"
            , hash
            , "--check-drep-metadata-hash"
            , "--out-file"
            , outFile
            ]
    )
