{-# LANGUAGE FlexibleContexts #-}

module Test.Cli.DRepMetadata where

import           Cardano.Api (MonadIO)

import           Control.Monad (void)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Trans.Control (MonadBaseControl)

import           Test.Cardano.CLI.Hash (exampleAnchorDataHash, exampleAnchorDataIpfsHash,
                   exampleAnchorDataPathTest, serveFilesWhile, tamperBase16Hash)
import           Test.Cardano.CLI.Util (execCardanoCLIWithEnvVars, expectFailure, propertyOnce)

import           Hedgehog (Property)
import qualified Hedgehog as H
import           Hedgehog.Internal.Property (MonadTest)

-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/drep metadata hash url wrong hash fails/"'@
hprop_drep_metadata_hash_url_wrong_hash_fails :: Property
hprop_drep_metadata_hash_url_wrong_hash_fails =
  propertyOnce . expectFailure $ do
    -- We modify the hash slightly so that the hash check fails
    alteredHash <- H.evalMaybe $ tamperBase16Hash exampleAnchorDataHash
    -- We run the test with the modified hash
    baseDrepMetadataHashUrl alteredHash

-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/drep metadata hash url correct hash/"'@
hprop_drep_metadata_hash_url_correct_hash :: Property
hprop_drep_metadata_hash_url_correct_hash =
  propertyOnce $ baseDrepMetadataHashUrl exampleAnchorDataHash

baseDrepMetadataHashUrl
  :: (MonadBaseControl IO m, MonadTest m, MonadIO m, MonadCatch m)
  => String
  -> m ()
baseDrepMetadataHashUrl hash = do
  let relativeUrl = ["ipfs", exampleAnchorDataIpfsHash]

  -- Create temporary HTTP server with files required by the call to `cardano-cli`
  -- In this case, the server emulates an IPFS gateway
  serveFilesWhile
    [ (relativeUrl, exampleAnchorDataPathTest)
    ]
    ( \port -> do
        void $
          execCardanoCLIWithEnvVars
            [("IPFS_GATEWAY_URI", "http://localhost:" ++ show port ++ "/")]
            [ "conway"
            , "governance"
            , "drep"
            , "metadata-hash"
            , "--drep-metadata-url"
            , "ipfs://" ++ exampleAnchorDataIpfsHash
            , "--expected-hash"
            , hash
            ]
    )
