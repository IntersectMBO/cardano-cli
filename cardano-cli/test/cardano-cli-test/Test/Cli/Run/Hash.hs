{-# LANGUAGE FlexibleContexts #-}

module Test.Cli.Run.Hash where

import Control.Monad (void)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource)
import GHC.Stack

import Test.Cardano.CLI.Util

import Hedgehog (MonadTest, Property)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H

hprop_hash_trip :: Property
hprop_hash_trip =
  watchdogProp . propertyOnce $ do
    hashTripFun "foo"
    hashTripFun "longerText"
    hashTripFun "nonAscii: 你好"
    hashTripFun "nonAscii: à la mode de Cæn"

-- Test that @cardano-cli hash --text > file1@ and
-- @cardano-cli --text --out-file file2@ yields
-- similar @file1@ and @file2@ files.
hashTripFun
  :: ( MonadBaseControl IO m
     , MonadTest m
     , MonadCatch m
     , MonadResource m
     , H.MonadAssertion m
     , HasCallStack
     )
  => String -> m ()
hashTripFun input =
  H.moduleWorkspace "tmp" $ \tempDir -> do
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
