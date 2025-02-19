{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Byron.UpdateProposal where

import Cardano.Api

import Cardano.CLI.Byron.UpdateProposal

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog (Property, (===))
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Internal.Property (failWith)

{- HLINT ignore "Use camelCase" -}

hprop_byron_update_proposal :: Property
hprop_byron_update_proposal = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  expectedUpdateProposal <- noteInputFile "test/cardano-cli-golden/files/input/byron/update-proposal"
  signingKey <- noteInputFile "test/cardano-cli-golden/files/input/byron/keys/byron.skey"
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

  eExpected <- liftIO . runExceptT $ readByronUpdateProposal expectedUpdateProposal
  expected <- case eExpected of
    Left err -> failWith Nothing . docToString $ renderByronUpdateProposalError err
    Right prop -> return prop

  eCreated <- liftIO . runExceptT $ readByronUpdateProposal createdUpdateProposal
  created <- case eCreated of
    Left err -> failWith Nothing . docToString $ renderByronUpdateProposalError err
    Right prop -> return prop

  expected === created
