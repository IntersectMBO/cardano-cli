module Test.Golden.Governance.Vote where

import Control.Monad (void)
import Data.Monoid (Last (..))
import GHC.IO.Exception (ExitCode (..))
import System.Environment qualified as IO

import Test.Cardano.CLI.Hash
  ( exampleAnchorDataHash
  , exampleAnchorDataIpfsHash
  , exampleAnchorDataPathGolden
  , serveFilesWhile
  , tamperBase16Hash
  )
import Test.Cardano.CLI.Util
  ( execCardanoCLI
  , execDetailConfigCardanoCLI
  , noteInputFile
  , propertyOnce
  , watchdogProp
  )

import Hedgehog (Property, (===))
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H

hprop_golden_governance_governance_vote_create :: Property
hprop_golden_governance_governance_vote_create =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    vkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/drep.vkey"
    voteFile <- H.noteTempFile tempDir "vote"
    voteGold <- H.note "test/cardano-cli-golden/files/golden/governance/vote/vote"

    void $
      execCardanoCLI
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
        , "https://example.com/vote"
        , "--anchor-data-hash"
        , "6163683a90d8cb460a38cdcf0d7bab286f0f004ec6e761dc670c2ca4d3709a17"
        ]

    H.diffFileVsGoldenFile voteFile voteGold

voteViewJsonFile :: FilePath
voteViewJsonFile = "test/cardano-cli-golden/files/golden/governance/vote/voteViewJSON"

hprop_golden_governance_governance_vote_view_json_stdout :: Property
hprop_golden_governance_governance_vote_view_json_stdout =
  watchdogProp . propertyOnce $ do
    voteFile <- noteInputFile "test/cardano-cli-golden/files/input/governance/vote/vote"
    H.noteShow_ voteViewJsonFile
    voteView <-
      execCardanoCLI
        [ "conway"
        , "governance"
        , "vote"
        , "view"
        , "--vote-file"
        , voteFile
        ]

    H.diffVsGoldenFile voteView voteViewJsonFile

hprop_golden_governance_governance_vote_view_json_outfile :: Property
hprop_golden_governance_governance_vote_view_json_outfile =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    voteFile <- noteInputFile "test/cardano-cli-golden/files/input/governance/vote/vote"
    voteViewFile <- H.noteTempFile tempDir "voteView"
    H.noteShow_ voteViewJsonFile
    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "vote"
        , "view"
        , "--vote-file"
        , voteFile
        , "--out-file"
        , voteViewFile
        ]

    H.diffFileVsGoldenFile voteViewFile voteViewJsonFile

hprop_golden_governance_governance_vote_view_yaml :: Property
hprop_golden_governance_governance_vote_view_yaml =
  watchdogProp . propertyOnce $ do
    voteFile <- noteInputFile "test/cardano-cli-golden/files/input/governance/vote/vote"
    voteViewGold <- H.note "test/cardano-cli-golden/files/golden/governance/vote/voteViewYAML"
    voteView <-
      execCardanoCLI
        [ "conway"
        , "governance"
        , "vote"
        , "view"
        , "--output-yaml"
        , "--vote-file"
        , voteFile
        ]

    H.diffVsGoldenFile voteView voteViewGold

hprop_golden_governance_governance_vote_create_yes_cc_hot_key :: Property
hprop_golden_governance_governance_vote_create_yes_cc_hot_key =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    ccVkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/cc.vkey"
    voteFile <- H.noteTempFile tempDir "vote"
    voteGold <- H.note "test/cardano-cli-golden/files/golden/governance/vote/vote_cc_yes.json"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "vote"
        , "create"
        , "--yes"
        , "--governance-action-tx-id"
        , "d21d997b5dbdd90180b642c3f4f2653cea629f6134cd9dc820d0fe6f11b54af4"
        , "--governance-action-index"
        , "0"
        , "--cc-hot-verification-key-file"
        , ccVkeyFile
        , "--out-file"
        , voteFile
        ]

    H.diffFileVsGoldenFile voteFile voteGold

hprop_golden_governance_governance_vote_create_no_cc_hot_key :: Property
hprop_golden_governance_governance_vote_create_no_cc_hot_key =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    ccVkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/cc.vkey"
    voteFile <- H.noteTempFile tempDir "vote"
    voteGold <- H.note "test/cardano-cli-golden/files/golden/governance/vote/vote_cc_no.json"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "vote"
        , "create"
        , "--no"
        , "--governance-action-tx-id"
        , "d21d997b5dbdd90180b642c3f4f2653cea629f6134cd9dc820d0fe6f11b54af4"
        , "--governance-action-index"
        , "0"
        , "--cc-hot-verification-key-file"
        , ccVkeyFile
        , "--out-file"
        , voteFile
        ]

    H.diffFileVsGoldenFile voteFile voteGold

hprop_golden_governance_governance_vote_create_abstain_cc_hot_key :: Property
hprop_golden_governance_governance_vote_create_abstain_cc_hot_key =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    ccVkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/cc.vkey"
    voteFile <- H.noteTempFile tempDir "vote"
    voteGold <- H.note "test/cardano-cli-golden/files/golden/governance/vote/vote_cc_abstain.json"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "vote"
        , "create"
        , "--abstain"
        , "--governance-action-tx-id"
        , "d21d997b5dbdd90180b642c3f4f2653cea629f6134cd9dc820d0fe6f11b54af4"
        , "--governance-action-index"
        , "0"
        , "--cc-hot-verification-key-file"
        , ccVkeyFile
        , "--out-file"
        , voteFile
        ]

    H.diffFileVsGoldenFile voteFile voteGold

-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/golden governance vote create wrong hash fails/"'@
hprop_golden_governance_vote_create_hash_fails :: Property
hprop_golden_governance_vote_create_hash_fails =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- We modify the hash slightly so that the hash check fails
    alteredHash <- H.evalMaybe $ tamperBase16Hash exampleAnchorDataHash
    let relativeUrl = ["ipfs", exampleAnchorDataIpfsHash]

    vkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/drep.vkey"
    voteFile <- H.noteTempFile tempDir "vote"
    voteGold <-
      H.note "test/cardano-cli-golden/files/golden/governance/vote/governance_vote_create_hash_fails.out"

    -- Create temporary HTTP server with files required by the call to `cardano-cli`
    env <- H.evalIO IO.getEnvironment
    (exitCode, _, result) <-
      serveFilesWhile
        [ (relativeUrl, exampleAnchorDataPathGolden)
        ]
        ( \port -> do
            execDetailConfigCardanoCLI
              ( H.defaultExecConfig
                  { H.execConfigEnv = Last $ Just (("IPFS_GATEWAY_URI", "http://localhost:" ++ show port ++ "/") : env)
                  }
              )
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
              , alteredHash
              , "--check-anchor-data-hash"
              ]
        )

    exitCode === ExitFailure 1

    H.diffVsGoldenFileExcludeTrace result voteGold
