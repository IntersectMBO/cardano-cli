{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.CLI.Effectful.File
  ( assertFileOccurences
  )
where

import Data.List qualified as L
import GHC.Stack qualified as GHC

import Hedgehog

import Effectful (Eff)
import Effectful.Zoo.Concurrent
import Effectful.Zoo.Core
import Effectful.Zoo.Error.Static
import Effectful.Zoo.FileSystem
import Effectful.Zoo.Hedgehog
import Effectful.Zoo.Hedgehog.Effect.Hedgehog
import Effectful.Zoo.Log.Static
import HaskellWorks.Prelude

-- | Assert the file contains the given number of occurrences of the given string
assertFileOccurences
  :: ()
  => r <: Concurrent
  => r <: Error Failure
  => r <: FileSystem
  => r <: Hedgehog
  => r <: Log Text
  => HasCallStack
  => Int
  -> String
  -> FilePath
  -> Eff r ()
assertFileOccurences n s fp =
  GHC.withFrozenCallStack $ do
    contents <-
      readStringFile fp
        & trapFail @IOException

    L.length (L.filter (s `L.isInfixOf`) (L.lines contents)) === n
