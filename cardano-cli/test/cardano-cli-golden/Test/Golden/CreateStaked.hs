{-# LANGUAGE ScopedTypeVariables #-}

module Test.Golden.CreateStaked where

import Cardano.Api

import Control.Monad (filterM, void)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.List (intercalate, sort)
import System.Directory
import System.FilePath

import Test.Cardano.CLI.Util (execCardanoCLI)

import Hedgehog (Property)
import Hedgehog qualified as H
import Hedgehog.Extras (moduleWorkspace, propertyOnce)
import Hedgehog.Extras qualified as H

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

hprop_golden_create_staked :: Property
hprop_golden_create_staked =
  propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
    let alonzo = "genesis.alonzo.spec.json"
        conway = "genesis.conway.spec.json"
        networkMagic = 42
        numPools = 4
        numStake = 8

    liftIO $ copyFile ("test/cardano-cli-golden/files/input/alonzo/" <> alonzo) (tempDir </> alonzo)
    liftIO $ copyFile ("test/cardano-cli-golden/files/input/conway/" <> conway) (tempDir </> conway)

    void $ H.note (tempDir </> alonzo)
    void $ H.note (tempDir </> conway)

    void $
      execCardanoCLI
        [ "conway"
        , "genesis"
        , "create-staked"
        , "--gen-genesis-keys"
        , "2"
        , "--gen-pools"
        , show numPools
        , "--gen-utxo-keys"
        , "3"
        , "--gen-stake-delegs"
        , show numStake
        , "--supply"
        , "1000000000000"
        , "--supply-delegated"
        , "1000000000000"
        , "--testnet-magic"
        , show networkMagic
        , "--bulk-pool-cred-files"
        , "2"
        , "--bulk-pools-per-file"
        , "2"
        , "--num-stuffed-utxo"
        , "7"
        , "--genesis-dir"
        , tempDir
        ]

    generated <- liftIO $ tree tempDir
    -- Sort output for stability, and make relative to avoid storing
    -- a path that changes everytime (/tmp/nix-shell.[0-9]+/tmp-Test...)
    let generated' = intercalate "\n" $ sort $ map (makeRelative tempDir) generated
        -- On Windows, the path separator is backslash. Normalize it to slash, like on Unix
        -- so that this test can run on all platforms.
        generated'' = map (\c -> if c == '\\' then '/' else c) generated'
    void $ H.note generated''

    H.diffVsGoldenFile generated'' "test/cardano-cli-golden/files/golden/conway/create-staked.out"

    bs <- liftIO $ LBS.readFile $ tempDir </> "genesis.json"
    genesis :: ShelleyGenesis <- Aeson.throwDecode bs

    H.assert (sgNetworkMagic genesis == networkMagic)
    H.assert ((length . sgsPools . sgStaking $ genesis) == numPools)
    H.assert ((length . sgsStake . sgStaking $ genesis) == numStake)
