{-# LANGUAGE ScopedTypeVariables #-}
module Test.Golden.CreateTestnetData where

import           Cardano.Api.Ledger (StandardCrypto)
import           Cardano.Api.Shelley (ShelleyGenesis (..))

import           Cardano.Ledger.Shelley.API (ShelleyGenesisStaking (..))

import           Control.Monad (filterM, void)
import           Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.List (intercalate, sort)
import           Data.Word (Word32)
import           System.Directory
import           System.Directory.Extra (listDirectories)
import           System.FilePath

import           Test.Cardano.CLI.Util (execCardanoCLI)

import           Hedgehog (Property)
import qualified Hedgehog as H
import           Hedgehog.Extras (moduleWorkspace, propertyOnce)
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Test.Golden as H

{- HLINT ignore "Use camelCase" -}

networkMagic :: Word32
networkMagic = 42

numDReps :: Int
numDReps = 5

numPools :: Int
numPools = 2

numUtxoKeys :: Int
numUtxoKeys = 3

-- | A function to create the arguments, so that they are shared
-- between the two tests, except for the possibly transient ones.
mkArguments :: String -> [String]
mkArguments outputDir =
  ["conway",  "genesis", "create-testnet-data"
   , "--genesis-keys", "2"
   , "--utxo-keys", show numUtxoKeys
   , "--out-dir", outputDir
   , "--testnet-magic", show networkMagic
   , "--pools", show numPools
   , "--drep-keys", show numDReps
  ]

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

-- | This test tests the non-transient case, i.e. it maximizes the files
-- that can be written to disk. Execute this test with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden create testnet data/'@
hprop_golden_create_testnet_data :: Property
hprop_golden_create_testnet_data =
  propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do

    let outputDir = tempDir </> "out"

    void $ execCardanoCLI $ mkArguments outputDir <> ["--stake-delegators", "4"]

    generated <- liftIO $ tree outputDir
    -- Sort output for stability, and make relative to avoid storing
    -- a path that changes everytime (/tmp/nix-shell.[0-9]+/tmp-Test...)
    let generated' = intercalate "\n" $ sort $ map (makeRelative outputDir) generated
        -- On Windows, the path separator is backslash. Normalize it to slash, like on Unix
        -- so that this test can run on all platforms.
        generated'' = map (\c -> if c == '\\' then '/' else c) generated'
    void $ H.note generated''

    H.diffVsGoldenFile generated'' "test/cardano-cli-golden/files/golden/conway/create-testnet-data.out"

    bs <- liftIO $ LBS.readFile $ outputDir </> "genesis.json"
    genesis :: ShelleyGenesis StandardCrypto <- Aeson.throwDecode bs

    H.assert (sgNetworkMagic genesis == networkMagic)
    H.assert ((length . sgsPools . sgStaking $ genesis) == numPools)

    actualNumDReps <- liftIO $ listDirectories $ outputDir </> "drep-keys"
    H.assert $ length actualNumDReps == numDReps

    actualNumUtxoKeys <- liftIO $ listDirectories $ outputDir </> "utxo-keys"
    H.assert $ length actualNumUtxoKeys == numUtxoKeys

-- | This test tests the transient case, i.e. it writes strictly
-- less things to disk than 'hprop_golden_create_testnet_data'. Execute this test with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden create testnet data transient stake delegators/'@
hprop_golden_create_testnet_data_transient_stake_delegators :: Property
hprop_golden_create_testnet_data_transient_stake_delegators =
  propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do

    let outputDir = tempDir </> "out"

    void $ execCardanoCLI $ mkArguments outputDir <> ["--transient-stake-delegators", "4"]

    -- We just test that the command doesn't crash when we execute a different path.
    -- For the golden part of this test, we are anyway covered by 'hprop_golden_create_testnet_data'
    -- that generates strictly more stuff.
