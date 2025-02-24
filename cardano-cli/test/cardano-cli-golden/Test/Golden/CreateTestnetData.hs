{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Test.Golden.CreateTestnetData where

import Cardano.Api
import Cardano.Api.Ledger (ConwayGenesis (..))
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley (ShelleyGenesis (..))

import Control.Monad
import Data.List (intercalate, sort)
import Data.Sequence.Strict qualified as Seq
import Data.Word (Word32)
import GHC.Exts (IsList (..))
import System.Directory
import System.Directory.Extra (listDirectories)
import System.FilePath

import Test.Cardano.CLI.Aeson
import Test.Cardano.CLI.Util (execCardanoCLI)

import Hedgehog (Property)
import Hedgehog qualified as H
import Hedgehog.Extras (moduleWorkspace, propertyOnce)
import Hedgehog.Extras qualified as H
import Hedgehog.Extras.Test.Golden qualified as H

{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use head" -}

networkMagic :: Word32
networkMagic = 623

numCommitteeKeys :: Int
numCommitteeKeys = 7

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
  [ "conway"
  , "genesis"
  , "create-testnet-data"
  , "--genesis-keys"
  , "2"
  , "--utxo-keys"
  , show numUtxoKeys
  , "--out-dir"
  , outputDir
  , "--testnet-magic"
  , show networkMagic
  , "--pools"
  , show numPools
  , "--drep-keys"
  , show numDReps
  , "--committee-keys"
  , show numCommitteeKeys
  , -- Relays file specifies two relays, like the number of SPOs
    "--relays"
  , "test/cardano-cli-golden/files/input/shelley/genesis/relays.json"
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

-- Execute this test with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden create testnet data/"'@
hprop_golden_create_testnet_data :: Property
hprop_golden_create_testnet_data =
  golden_create_testnet_data Nothing

-- Execute this test with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden create testnet data with template/"'@
hprop_golden_create_testnet_data_with_template :: Property
hprop_golden_create_testnet_data_with_template =
  golden_create_testnet_data $
    Just "test/cardano-cli-golden/files/input/shelley/genesis/genesis.spec.json"

createTestnetDataOutGoldenFile :: FilePath
createTestnetDataOutGoldenFile = "test/cardano-cli-golden/files/golden/conway/create-testnet-data.out"

-- | This test tests the non-transient case, i.e. it maximizes the files
-- that can be written to disk.
golden_create_testnet_data
  :: ()
  => Maybe FilePath
  -- ^ The path to the shelley template use, if any
  -> Property
golden_create_testnet_data mShelleyTemplate =
  propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
    let outputDir = tempDir </> "out"
        templateArg :: [String] =
          case mShelleyTemplate of
            Nothing -> []
            Just shelleyTemplate -> ["--spec-shelley", shelleyTemplate]
        numStakeDelegs = 4

    void $
      execCardanoCLI $
        mkArguments outputDir <> ["--stake-delegators", show numStakeDelegs] <> templateArg

    generated <- liftIO $ tree outputDir
    -- Sort output for stability, and make relative to avoid storing
    -- a path that changes everytime (/tmp/nix-shell.[0-9]+/tmp-Test...)
    let generated' = intercalate "\n" $ sort $ map (makeRelative outputDir) generated
        -- On Windows, the path separator is backslash. Normalize it to slash, like on Unix
        -- so that this test can run on all platforms.
        generated'' = map (\c -> if c == '\\' then '/' else c) generated'
    void $ H.note generated''

    H.diffVsGoldenFile generated'' createTestnetDataOutGoldenFile

    shelleyGenesis :: ShelleyGenesis <-
      H.readJsonFileOk $ outputDir </> "shelley-genesis.json"

    sgNetworkMagic shelleyGenesis H.=== networkMagic
    length (L.sgsPools $ sgStaking shelleyGenesis) H.=== numPools

    forM_ (L.sgsPools $ sgStaking shelleyGenesis) $ \pool ->
      Seq.length (L.ppRelays pool) H.=== 1

    actualNumCCs <- liftIO $ listDirectories $ outputDir </> "cc-keys"
    length actualNumCCs H.=== numCommitteeKeys

    actualNumDReps <- liftIO $ listDirectories $ outputDir </> "drep-keys"
    length actualNumDReps H.=== numDReps

    actualNumUtxoKeys <- liftIO $ listDirectories $ outputDir </> "utxo-keys"
    length actualNumUtxoKeys H.=== numUtxoKeys

    conwayGenesis :: ConwayGenesis <-
      H.readJsonFileOk $ outputDir </> "conway-genesis.json"

    length (L.committeeMembers $ cgCommittee conwayGenesis) H.=== numCommitteeKeys

    length (cgInitialDReps conwayGenesis) H.=== numDReps

    length (cgDelegs conwayGenesis) H.=== numStakeDelegs

-- Execute this test with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden create testnet data deleg non deleg/"'@
hprop_golden_create_testnet_data_deleg_non_deleg :: Property
hprop_golden_create_testnet_data_deleg_non_deleg =
  propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
    let outputDir = tempDir </> "out"
        totalSupply :: Int = 2_000_000_000_000 -- 2*10^12
        delegatedSupply :: Int = 500_000_000_000 -- 5*10^11, i.e. totalSupply / 4
    void $
      execCardanoCLI
        [ "conway"
        , "genesis"
        , "create-testnet-data"
        , "--utxo-keys"
        , "1"
        , "--total-supply"
        , show totalSupply
        , "--delegated-supply"
        , show delegatedSupply
        , "--out-dir"
        , outputDir
        ]

    genesis :: ShelleyGenesis <- H.readJsonFileOk $ outputDir </> "shelley-genesis.json"

    -- Because we don't test this elsewhere in this file:
    (sgMaxLovelaceSupply genesis) H.=== (fromIntegral totalSupply)

    let initialFunds = toList $ sgInitialFunds genesis
    -- This checks that there is actually only one funded address
    (length initialFunds) H.=== 1

    let L.Coin onlyHolderCoin = snd $ initialFunds !! 0

    -- The check below may seem weird, but we cannot do a very precise check
    -- on balances, because of the treasury "stealing" some of the money.
    -- Nevertheless, this check catches a confusion between delegated and
    -- non-delegated coins, by virtue of --delegated-supply being a fourth
    -- of --total-supply above. This confusion actually happened in the past, as
    -- https://github.com/IntersectMBO/cardano-cli/issues/631 witnesses.

    H.assertWith (fromIntegral onlyHolderCoin) (\ohc -> ohc > totalSupply `div` 2)

-- Execute this test with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden create testnet data shelley genesis output/"'@
--
-- This test checks that we use the total supply value from the shelley template when --total-supply
-- is not specified. It was broken in the past (see https://github.com/IntersectMBO/cardano-node/issues/5953).
hprop_golden_create_testnet_data_shelley_genesis_output :: Property
hprop_golden_create_testnet_data_shelley_genesis_output =
  propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
    vanillaShelleyGenesis :: ShelleyGenesis <-
      H.readJsonFileOk "test/cardano-cli-golden/files/input/shelley/genesis/genesis.spec.json"
    let tweakedValue = 3_123_456_000_000
        tweakedShelleyGenesis = vanillaShelleyGenesis{sgMaxLovelaceSupply = tweakedValue}
        tweakedShelleyGenesisFp = tempDir </> "tweaked-shelley-genesis.json"

    void $ liftIO $ writeFileJSON tweakedShelleyGenesisFp tweakedShelleyGenesis

    let outputDir = tempDir </> "out"
    void $
      execCardanoCLI
        [ "conway"
        , "genesis"
        , "create-testnet-data"
        , "--spec-shelley"
        , tweakedShelleyGenesisFp
        , "--out-dir"
        , outputDir
        ]

    let outputGenesisFp = outputDir </> "shelley-genesis.json"
        redactedOutputGenesisFp = outputDir </> "shelley-genesis-redacted.json"

    redactJsonFieldsInFile
      (fromList $ map (,"<redacted>") ["genDelegs", "systemStart"])
      outputGenesisFp
      redactedOutputGenesisFp

    H.diffFileVsGoldenFile
      redactedOutputGenesisFp
      "test/cardano-cli-golden/files/golden/conway/custom-lovelace-supply-shelley-genesis.json"
