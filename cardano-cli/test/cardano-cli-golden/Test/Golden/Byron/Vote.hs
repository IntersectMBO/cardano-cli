{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Byron.Vote where

import Cardano.CLI.Byron.Vote

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Text as Text

import Test.Cardano.CLI.Util

import Hedgehog (Property, (===))
import qualified Hedgehog.Extras.Test.Base as H
import Hedgehog.Internal.Property (failWith)

{- HLINT ignore "Use camelCase" -}

hprop_golden_byron_yes_vote :: Property
hprop_golden_byron_yes_vote = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  goldenYesVote <- noteInputFile "test/cardano-cli-golden/files/golden/byron/votes/vote-yes"
  proposal <- noteInputFile "test/cardano-cli-golden/files/golden/byron/update-proposal"
  signingKey <- noteInputFile "test/cardano-cli-golden/files/golden/byron/keys/byron.skey"
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

  eGolden <- liftIO . runExceptT $ readByronVote goldenYesVote
  golden <- case eGolden of
    Left err -> failWith Nothing . Text.unpack $ renderByronVoteError err
    Right prop -> return prop

  eCreated <- liftIO . runExceptT $ readByronVote createdYesVote
  created <- case eCreated of
    Left err -> failWith Nothing . Text.unpack $ renderByronVoteError err
    Right prop -> return prop

  golden === created

hprop_golden_byron_no_vote :: Property
hprop_golden_byron_no_vote = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  goldenNoVote <- noteInputFile "test/cardano-cli-golden/files/golden/byron/votes/vote-no"
  proposal <- noteInputFile "test/cardano-cli-golden/files/golden/byron/update-proposal"
  signingKey <- noteInputFile "test/cardano-cli-golden/files/golden/byron/keys/byron.skey"
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

  eGolden <- liftIO . runExceptT $ readByronVote goldenNoVote
  golden <- case eGolden of
    Left err -> failWith Nothing . Text.unpack $ renderByronVoteError err
    Right prop -> return prop

  eCreated <- liftIO . runExceptT $ readByronVote createdNoVote
  created <- case eCreated of
    Left err -> failWith Nothing . Text.unpack $ renderByronVoteError err
    Right prop -> return prop

  golden === created
