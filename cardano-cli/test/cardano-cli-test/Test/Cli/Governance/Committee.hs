{-# LANGUAGE FlexibleContexts #-}

module Test.Cli.Governance.Committee where

import Cardano.Api (MonadIO)

import Control.Monad (void)
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
  , noteTempFile
  , propertyOnce
  , watchdogProp
  )
import Test.Cardano.CLI.Workspace

import Hedgehog (MonadTest, Property)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/governance committee checks wrong hash fails/"'@
hprop_governance_committee_checks_wrong_hash_fails :: Property
hprop_governance_committee_checks_wrong_hash_fails =
  watchdogProp . propertyOnce . moduleWorkspace2 "tmp" $ \tempDir -> H.assertFailure_ $ do
    -- We modify the hash slightly so that the hash check fails
    alteredHash <- H.evalMaybe $ tamperBase16Hash exampleAnchorDataHash
    -- We run the test with the altered
    baseGovernanceGovernanceCommitteeChecksHash
      alteredHash
      tempDir

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/governance committee checks right hash works/"'@
hprop_governance_committee_checks_right_hash_works :: Property
hprop_governance_committee_checks_right_hash_works =
  watchdogProp . propertyOnce . moduleWorkspace2 "tmp" $ \tempDir ->
    baseGovernanceGovernanceCommitteeChecksHash exampleAnchorDataHash tempDir

baseGovernanceGovernanceCommitteeChecksHash
  :: (MonadBaseControl IO m, MonadTest m, MonadIO m, MonadCatch m) => String -> FilePath -> m ()
baseGovernanceGovernanceCommitteeChecksHash hash tempDir = do
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
            , "committee"
            , "create-cold-key-resignation-certificate"
            , "--cold-verification-key-file"
            , ccColdVKey
            , "--resignation-metadata-url"
            , "ipfs://" ++ exampleAnchorDataIpfsHash
            , "--resignation-metadata-hash"
            , hash
            , "--check-resignation-metadata-hash"
            , "--out-file"
            , certFile
            ]
    )
