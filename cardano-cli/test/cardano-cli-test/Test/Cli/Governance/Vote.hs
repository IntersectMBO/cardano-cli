{-# LANGUAGE FlexibleContexts #-}

module Test.Cli.Governance.Vote where

import           Cardano.Api (MonadIO)

import           Control.Monad (void)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Trans.Control (MonadBaseControl)

import           Test.Cardano.CLI.Hash (exampleAnchorDataHash, exampleAnchorDataIpfsHash,
                   exampleAnchorDataPathTest, serveFilesWhile, tamperBase16Hash)
import           Test.Cardano.CLI.Util (execCardanoCLIWithEnvVars, expectFailure, noteInputFile,
                   propertyOnce)

import           Hedgehog (MonadTest, Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/governance vote create wrong hash fails/"'@
hprop_governance_vote_create_wrong_hash_fails :: Property
hprop_governance_vote_create_wrong_hash_fails =
  propertyOnce . expectFailure . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- We modify the hash slightly so that the hash check fails
    alteredHash <- H.evalMaybe $ tamperBase16Hash exampleAnchorDataHash
    -- We run the test with the altered
    baseGovernanceVoteCreateHashCheck
      alteredHash
      tempDir

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/governance vote create right hash works/"'@
hprop_governance_vote_create_right_hash_works :: Property
hprop_governance_vote_create_right_hash_works =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir ->
    baseGovernanceVoteCreateHashCheck exampleAnchorDataHash tempDir

baseGovernanceVoteCreateHashCheck
  :: (MonadBaseControl IO m, MonadTest m, MonadIO m, MonadCatch m) => String -> FilePath -> m ()
baseGovernanceVoteCreateHashCheck hash tempDir = do
  vkeyFile <- noteInputFile "test/cardano-cli-test/files/input/drep.vkey"
  voteFile <- H.noteTempFile tempDir "vote"

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
            , "vote"
            , "create"
            , "--yes"
            , "--governance-action-tx-id"
            , "b1015258a99351c143a7a40b7b58f033ace10e3cc09c67780ed5b2b0992aa60a"
            , "--governance-action-index"
            , "5"
            , "--drep-verification-key-file"
            , vkeyFile
            , "--out-file"
            , voteFile
            , "--anchor-url"
            , "ipfs://" ++ exampleAnchorDataIpfsHash
            , "--anchor-data-hash"
            , hash
            , "--check-anchor-data-hash"
            ]
    )
