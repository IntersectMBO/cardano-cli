{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Byron.Vote where

import Cardano.Api

import Cardano.CLI.Byron.Vote

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog ((===))
import Hedgehog.Extras (UnitIO)
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Internal.Property (failWith)

{- HLINT ignore "Use camelCase" -}

tasty_byron_yes_vote :: UnitIO ()
tasty_byron_yes_vote =
  H.moduleWorkspace "tmp" $ \tempDir -> do
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

    eExpected <- liftIO . runExceptT $ readByronVote expectedYesVote
    expected <- case eExpected of
      Left err -> failWith Nothing . docToString $ renderByronVoteError err
      Right prop -> return prop

    eCreated <- liftIO . runExceptT $ readByronVote createdYesVote
    created <- case eCreated of
      Left err -> failWith Nothing . docToString $ renderByronVoteError err
      Right prop -> return prop

    expected === created

tasty_byron_no_vote :: UnitIO ()
tasty_byron_no_vote =
  H.moduleWorkspace "tmp" $ \tempDir -> do
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

    eExpected <- liftIO . runExceptT $ readByronVote expectedNoVote
    expected <- case eExpected of
      Left err -> failWith Nothing . docToString $ renderByronVoteError err
      Right prop -> return prop

    eCreated <- liftIO . runExceptT $ readByronVote createdNoVote
    created <- case eCreated of
      Left err -> failWith Nothing . docToString $ renderByronVoteError err
      Right prop -> return prop

    expected === created
