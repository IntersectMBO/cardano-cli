{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Byron.Vote where

import Cardano.CLI.Byron.Vote

import RIO

import Test.Cardano.CLI.Util

import Hedgehog (Property, (===))
import Hedgehog.Extras.Test.Base qualified as H

{- HLINT ignore "Use camelCase" -}

hprop_byron_yes_vote :: Property
hprop_byron_yes_vote =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    expectedYesVote <- noteInputFile "test/cardano-cli-golden/files/input/byron/votes/vote-yes"
    proposal <- noteInputFile "test/cardano-cli-golden/files/input/byron/update-proposal"
    signingKey <- noteInputFile "test/cardano-cli-golden/files/input/byron/keys/byron.skey"
    createdYesVote <- noteTempFile tempDir "byron-yes-vote"
    void $
      execCardanoCLI
        [ "byron"
        , "governance"
        , "create-proposal-vote"
        , "--mainnet"
        , "--proposal-filepath"
        , proposal
        , "--signing-key"
        , signingKey
        , "--vote-yes"
        , "--output-filepath"
        , createdYesVote
        ]

    expected <- runRIO () $ readByronVote expectedYesVote

    created <- runRIO () $ readByronVote createdYesVote

    expected === created

hprop_byron_no_vote :: Property
hprop_byron_no_vote =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    expectedNoVote <- noteInputFile "test/cardano-cli-golden/files/input/byron/votes/vote-no"
    proposal <- noteInputFile "test/cardano-cli-golden/files/input/byron/update-proposal"
    signingKey <- noteInputFile "test/cardano-cli-golden/files/input/byron/keys/byron.skey"
    createdNoVote <- noteTempFile tempDir "byron-no-vote"
    void $
      execCardanoCLI
        [ "byron"
        , "governance"
        , "create-proposal-vote"
        , "--mainnet"
        , "--proposal-filepath"
        , proposal
        , "--signing-key"
        , signingKey
        , "--vote-no"
        , "--output-filepath"
        , createdNoVote
        ]

    expected <- runRIO () $ readByronVote expectedNoVote

    created <- runRIO () $ readByronVote createdNoVote

    expected === created
