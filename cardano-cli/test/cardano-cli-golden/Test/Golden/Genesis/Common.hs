-- | Helpers shared between the genesis golden tests.
module Test.Golden.Genesis.Common
  ( tree
  )
where

import Control.Monad (filterM)
import System.Directory
import System.FilePath

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
