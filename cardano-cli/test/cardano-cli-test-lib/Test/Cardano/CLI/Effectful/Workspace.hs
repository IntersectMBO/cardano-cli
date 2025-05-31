{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Test.Cardano.CLI.Effectful.Workspace
  ( localWorkspace,
  ) where

import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Zoo.Core
import Effectful.Zoo.Environment
import Effectful.Zoo.Error.Static
import Effectful.Zoo.FileSystem
import Effectful.Zoo.Hedgehog.Api
import Effectful.Zoo.Hedgehog.Api.Workspace
import Effectful.Zoo.Hedgehog.Effect.Hedgehog
import Effectful.Zoo.Log.Static
import Effectful.Zoo.Reader.Static
import HaskellWorks.Prelude

localWorkspace :: ()
  => HasCallStack
  => r <: Concurrent
  => r <: Environment
  => r <: Error Failure
  => r <: FileSystem
  => r <: Hedgehog
  => r <: Log Text
  => Text
  -> Eff
        ( Reader Workspace
        : Reader ProjectRoot
        : Reader PackagePath
        : r)
        ()
  -> Eff r ()
localWorkspace prefix f =
  withFrozenCallStack $ do
    cabalProjectDir <- findCabalProjectDir "."

    f & moduleWorkspace (T.unpack prefix)
      & runReader (ProjectRoot cabalProjectDir)
      & runReader (PackagePath "cardano-cli")
