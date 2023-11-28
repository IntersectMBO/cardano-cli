module Test.Golden.CreateTestnetData where

import           Control.Monad (filterM, void)
import           Control.Monad.IO.Class
import           Data.List (intercalate, sort)
import           System.Directory
import           System.FilePath

import           Test.Cardano.CLI.Util (execCardanoCLI)

import           Hedgehog (Property)
import           Hedgehog.Extras (moduleWorkspace, propertyOnce)
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Test.Golden as H

{- HLINT ignore "Use camelCase" -}

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

hprop_golden_create_testnet_data :: Property
hprop_golden_create_testnet_data =
  propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do

    let outputDir = tempDir </> "out"

    void $
      execCardanoCLI
        ["conway",  "genesis", "create-testnet-data"
         , "--genesis-keys", "2"
         , "--utxo-keys", "3"
         , "--out-dir", outputDir
         , "--testnet-magic", "42"
         , "--pools", "2"
        ]

    generated <- liftIO $ tree outputDir
    -- Sort output for stability, and make relative to avoid storing
    -- a path that changes everytime (/tmp/nix-shell.[0-9]+/tmp-Test...)
    let generated' = intercalate "\n" $ sort $ map (makeRelative outputDir) generated
        -- On Windows, the path separator is backslash. Normalize it to slash, like on Unix
        -- so that this test can run on all platforms.
        generated'' = map (\c -> if c == '\\' then '/' else c) generated'
    void $ H.note generated''

    H.diffVsGoldenFile generated'' "test/cardano-cli-golden/files/golden/conway/create-testnet-data.out"
