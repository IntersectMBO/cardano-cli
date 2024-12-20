{-# LANGUAGE FlexibleContexts #-}

module Test.Cli.Governance.Committee where

import           Cardano.Api (MonadIO)

import           Control.Monad (void)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Trans.Control (MonadBaseControl)

import           Test.Cardano.CLI.Hash (AnchorDataExample (..), dummyAnchorDataExample1,
                   serveFilesWhile, tamperAnchorDataExampleHash)
import           Test.Cardano.CLI.Util (execCardanoCLI, execCardanoCLIWithEnvVars, expectFailure,
                   noteTempFile, propertyOnce)

import           Hedgehog (MonadTest, Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/governance committee checks wrong hash fails/"'@
hprop_governance_committee_checks_wrong_hash_fails :: Property
hprop_governance_committee_checks_wrong_hash_fails =
  propertyOnce . expectFailure . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- We modify the hash slightly so that the hash check fails
    alteredHash <- H.evalMaybe $ tamperAnchorDataExampleHash dummyAnchorDataExample1
    -- We run the test with the altered
    baseGovernanceGovernanceCommitteeChecksHash
      alteredHash
      tempDir

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/governance committee checks right hash works/"'@
hprop_governance_committee_checks_right_hash_works :: Property
hprop_governance_committee_checks_right_hash_works =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir ->
    baseGovernanceGovernanceCommitteeChecksHash dummyAnchorDataExample1 tempDir

baseGovernanceGovernanceCommitteeChecksHash
  :: (MonadBaseControl IO m, MonadTest m, MonadIO m, MonadCatch m)
  => AnchorDataExample -> FilePath -> m ()
baseGovernanceGovernanceCommitteeChecksHash exampleAnchorData tempDir = do
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

  let relativeUrl = ["ipfs", anchorDataIpfsHash exampleAnchorData]

  -- Create temporary HTTP server with files required by the call to `cardano-cli`
  -- In this case, the server emulates an IPFS gateway
  serveFilesWhile
    [(relativeUrl, anchorDataPathTest exampleAnchorData)]
    ( \port -> do
        void $
          execCardanoCLIWithEnvVars
            [("IPFS_GATEWAY_URI", "http://localhost:" ++ show port ++ "/")]
            [ "conway"
            , "governance"
            , "committee"
            , "create-cold-key-resignation-certificate"
            , "--cold-verification-key-file"
            , ccColdVKey
            , "--resignation-metadata-url"
            , "ipfs://" ++ anchorDataIpfsHash exampleAnchorData
            , "--resignation-metadata-hash"
            , anchorDataHash exampleAnchorData
            , "--check-resignation-metadata-hash"
            , "--out-file"
            , certFile
            ]
    )
