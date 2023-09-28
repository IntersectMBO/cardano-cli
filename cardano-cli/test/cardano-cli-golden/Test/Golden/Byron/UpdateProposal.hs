{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Byron.UpdateProposal where

import Cardano.CLI.Byron.UpdateProposal

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Text as Text

import Test.Cardano.CLI.Util

import Hedgehog (Property, (===))
import qualified Hedgehog.Extras.Test.Base as H
import Hedgehog.Internal.Property (failWith)

{- HLINT ignore "Use camelCase" -}

hprop_golden_byron_update_proposal :: Property
hprop_golden_byron_update_proposal = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  goldenUpdateProposal <- noteInputFile "test/cardano-cli-golden/files/golden/byron/update-proposal"
  signingKey <- noteInputFile "test/cardano-cli-golden/files/golden/byron/keys/byron.skey"
  createdUpdateProposal <- noteTempFile tempDir "byron-update-proposal"
  void $
    execCardanoCLI
      [ "byron"
      , "governance"
      , "create-update-proposal"
      , "--mainnet"
      , "--signing-key"
      , signingKey
      , "--protocol-version-major"
      , "1"
      , "--protocol-version-minor"
      , "0"
      , "--protocol-version-alt"
      , "0"
      , "--application-name"
      , "cardano-sl"
      , "--software-version-num"
      , "1"
      , "--system-tag"
      , "linux"
      , "--installer-hash"
      , "0"
      , "--filepath"
      , createdUpdateProposal
      ]

  eGolden <- liftIO . runExceptT $ readByronUpdateProposal goldenUpdateProposal
  golden <- case eGolden of
    Left err -> failWith Nothing . Text.unpack $ renderByronUpdateProposalError err
    Right prop -> return prop

  eCreated <- liftIO . runExceptT $ readByronUpdateProposal createdUpdateProposal
  created <- case eCreated of
    Left err -> failWith Nothing . Text.unpack $ renderByronUpdateProposalError err
    Right prop -> return prop

  golden === created
