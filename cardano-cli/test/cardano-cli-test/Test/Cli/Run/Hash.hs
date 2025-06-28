{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE FlexibleContexts #-}

module Test.Cli.Run.Hash where

import Control.Monad (void)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource)
import GHC.Stack

import Test.Cardano.CLI.Util
import Test.Cardano.CLI.Workspace

import Hedgehog (MonadTest, Property)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H

hprop_hash_trip :: Property
hprop_hash_trip =
  watchdogProp . propertyOnce $ do
    hash_trip_fun "foo"
    hash_trip_fun "longerText"
    hash_trip_fun "nonAscii: 你好"
    hash_trip_fun "nonAscii: à la mode de Cæn"

-- Test that @cardano-cli hash --text > file1@ and
-- @cardano-cli --text --out-file file2@ yields
-- similar @file1@ and @file2@ files.
hash_trip_fun
  :: HasCallStack
  => MonadBaseControl IO m
  => MonadCatch m
  => MonadResource m
  => MonadTest m
  => String
  -> m ()
hash_trip_fun input =
  moduleWorkspace2 "tmp" $ \tempDir -> do
    hashFile <- noteTempFile tempDir "hash.txt"

    hash <-
      execCardanoCLI
        [ "hash"
        , "anchor-data"
        , "--text"
        , input
        ]

    void $
      execCardanoCLI
        [ "hash"
        , "anchor-data"
        , "--text"
        , input
        , "--out-file"
        , hashFile
        ]

    hashFromFile <- H.readFile hashFile

    H.diff hash (==) hashFromFile
