{- HLINT ignore "Use camelCase" -}

module Test.Golden.Governance.Vote where

import           Control.Monad (void)

import           Test.Cardano.CLI.Util (execCardanoCLI, noteInputFile, propertyOnce)

import           Hedgehog
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Golden as H

hprop_golden_governance_governance_vote_create :: Property
hprop_golden_governance_governance_vote_create =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    vkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/drep.vkey"
    voteFile <- H.noteTempFile tempDir "vote"
    voteGold <- H.note "test/cardano-cli-golden/files/golden/governance/vote/vote"

    void $ execCardanoCLI
      [ "conway", "governance", "vote", "create"
      , "--yes"
      , "--governance-action-tx-id", "b1015258a99351c143a7a40b7b58f033ace10e3cc09c67780ed5b2b0992aa60a"
      , "--governance-action-index", "5"
      , "--drep-verification-key-file", vkeyFile
      , "--out-file", voteFile
      , "--vote-anchor-url", "https://example.com/vote"
      , "--vote-anchor-metadata", "I don't like this proposal, because it's bad. I'm not going to tell you why I voted yes nonetheless."
      ]

    H.diffFileVsGoldenFile voteFile voteGold

hprop_golden_governance_governance_vote_view_json_stdout :: Property
hprop_golden_governance_governance_vote_view_json_stdout =
  propertyOnce $ do
    voteFile <- noteInputFile "test/cardano-cli-golden/files/golden/governance/vote/vote"
    voteViewGold <- H.note "test/cardano-cli-golden/files/golden/governance/vote/voteViewJSON"
    voteView <- execCardanoCLI
      [ "conway", "governance", "vote", "view"
      , "--vote-file", voteFile
      ]

    H.diffVsGoldenFile voteView voteViewGold

hprop_golden_governance_governance_vote_view_json_outfile :: Property
hprop_golden_governance_governance_vote_view_json_outfile =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    voteFile <- noteInputFile "test/cardano-cli-golden/files/golden/governance/vote/vote"
    voteViewFile <- H.noteTempFile tempDir "voteView"
    voteViewGold <- H.note "test/cardano-cli-golden/files/golden/governance/vote/voteViewJSON"
    void $ execCardanoCLI
      [ "conway", "governance", "vote", "view"
      , "--vote-file", voteFile
      , "--out-file", voteViewFile
      ]

    H.diffFileVsGoldenFile voteViewFile voteViewGold

hprop_golden_governance_governance_vote_view_yaml :: Property
hprop_golden_governance_governance_vote_view_yaml =
  propertyOnce $ do
    voteFile <- noteInputFile "test/cardano-cli-golden/files/golden/governance/vote/vote"
    voteViewGold <- H.note "test/cardano-cli-golden/files/golden/governance/vote/voteViewYAML"
    voteView <- execCardanoCLI
      [ "conway", "governance", "vote", "view"
      , "--yaml"
      , "--vote-file", voteFile
      ]

    H.diffVsGoldenFile voteView voteViewGold

hprop_golden_governance_governance_vote_create_yes_cc_hot_key :: Property
hprop_golden_governance_governance_vote_create_yes_cc_hot_key =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    ccVkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/cc.vkey"
    voteFile <- H.noteTempFile tempDir "vote"
    voteGold <- H.note "test/cardano-cli-golden/files/golden/governance/vote/vote_cc_yes.json"

    void $ execCardanoCLI
      [ "conway", "governance", "vote", "create"
      , "--yes"
      , "--governance-action-tx-id", "d21d997b5dbdd90180b642c3f4f2653cea629f6134cd9dc820d0fe6f11b54af4"
      , "--governance-action-index", "0"
      , "--cc-hot-verification-key-file", ccVkeyFile
      , "--out-file", voteFile
      ]

    H.diffFileVsGoldenFile voteFile voteGold

hprop_golden_governance_governance_vote_create_no_cc_hot_key :: Property
hprop_golden_governance_governance_vote_create_no_cc_hot_key =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    ccVkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/cc.vkey"
    voteFile <- H.noteTempFile tempDir "vote"
    voteGold <- H.note "test/cardano-cli-golden/files/golden/governance/vote/vote_cc_no.json"

    void $ execCardanoCLI
      [ "conway", "governance", "vote", "create"
      , "--no"
      , "--governance-action-tx-id", "d21d997b5dbdd90180b642c3f4f2653cea629f6134cd9dc820d0fe6f11b54af4"
      , "--governance-action-index", "0"
      , "--cc-hot-verification-key-file", ccVkeyFile
      , "--out-file", voteFile
      ]

    H.diffFileVsGoldenFile voteFile voteGold

hprop_golden_governance_governance_vote_create_abstain_cc_hot_key :: Property
hprop_golden_governance_governance_vote_create_abstain_cc_hot_key =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    ccVkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/cc.vkey"
    voteFile <- H.noteTempFile tempDir "vote"
    voteGold <- H.note "test/cardano-cli-golden/files/golden/governance/vote/vote_cc_abstain.json"

    void $ execCardanoCLI
      [ "conway", "governance", "vote", "create"
      , "--abstain"
      , "--governance-action-tx-id", "d21d997b5dbdd90180b642c3f4f2653cea629f6134cd9dc820d0fe6f11b54af4"
      , "--governance-action-index", "0"
      , "--cc-hot-verification-key-file", ccVkeyFile
      , "--out-file", voteFile
      ]

    H.diffFileVsGoldenFile voteFile voteGold
