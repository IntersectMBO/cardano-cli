{-# LANGUAGE LambdaCase #-}

-- | Helpers shared between the genesis golden tests.
module Test.Golden.Genesis.Common
  ( tree
  , injectionToList
  )
where

-- For the orphan 'IsList (ListMap k v)' instance, which gives us 'toList' below.
import Cardano.Api ()

import Cardano.Ledger.Shelley.Genesis (InjectionData (..))

import Control.Monad (filterM)
import GHC.Exts (IsList (..))
import GHC.Stack (HasCallStack)
import System.Directory
import System.FilePath

import Hedgehog (MonadTest)
import Hedgehog qualified as H

-- | Given a root directory, returns files within this root (recursively)
tree :: FilePath -> IO [FilePath]
tree root = do
  -- listDirectory returns a path relative to 'root'. We need to prepend
  -- root to it for queries below.
  content <- map (root </>) <$> listDirectory root
  files <- filterM doesFileExist content
  subs <- filterM doesDirectoryExist content
  subTrees <- mapM tree subs
  return $ files ++ concat subTrees

-- | The key/value pairs held by an 'InjectionData'.
--
-- Genesis decoded from JSON in these tests only ever carries its data inline, so
-- 'InjectionFromFile' fails the test.
injectionToList
  :: (MonadTest m, HasCallStack)
  => InjectionData k v
  -> m [(k, v)]
injectionToList = \case
  NoInjection -> pure []
  EmbeddedInjection lm -> pure $ toList lm
  InjectionFromFile fp _ -> do
    H.annotate $ "unexpected InjectionFromFile in decoded genesis: " <> show fp
    H.failure
