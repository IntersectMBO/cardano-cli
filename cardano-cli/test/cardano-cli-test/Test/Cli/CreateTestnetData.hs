{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cli.CreateTestnetData where

import Cardano.Api

import Control.Monad (forM_, void)
import Data.List (isInfixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.IO.Exception (ExitCode (..))
import System.FilePath ((</>))

import Test.Cardano.CLI.Util
  ( assertDirectoryMissing
  , execCardanoCLI
  , execDetailCardanoCLI
  , watchdogProp
  )

import Hedgehog (Property, success, (===))
import Hedgehog.Extras (moduleWorkspace, propertyOnce)
import Hedgehog.Extras qualified as H

-- | Test case for https://github.com/IntersectMBO/cardano-cli/issues/587
-- Execute this test with:
-- @cabal test cardano-cli-test --test-options '-p "/create testnet data minimal/"'@
hprop_create_testnet_data_minimal :: Property
hprop_create_testnet_data_minimal =
  watchdogProp . propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
    let outputDir = tempDir </> "out"

    -- We test that the command doesn't crash, because otherwise
    -- execCardanoCLI would fail.
    H.noteM_ $
      execCardanoCLI
        [ "conway"
        , "genesis"
        , "create-testnet-data"
        , "--testnet-magic"
        , "42"
        , "--out-dir"
        , outputDir
        ]
    success

-- Execute this test with:
-- @cabal test cardano-cli-test --test-options '-p "/create testnet data create nonegative supply/"'@
hprop_create_testnet_data_create_nonegative_supply :: Property
hprop_create_testnet_data_create_nonegative_supply = do
  -- FIXME rewrite this as a property test
  let supplyValues =
        [ -- (total supply, delegated supply, exit code)
          (2_000_000_000, 1_000_000_000, ExitSuccess)
        , (1_100_000_000, 1_000_000_000, ExitSuccess)
        , (1_000_000_000, 1_000_000_000, ExitSuccess)
        , (1_000_000_000_000, 1_000_000_000, ExitSuccess)
        , (1_000_000_000, 1_000_000_001, ExitFailure 1)
        , (1_000_000_000, 1_100_000_001, ExitFailure 1)
        , (1_000_000_000, 2_000_000_000, ExitFailure 1)
        ]
          :: [(Int, Int, ExitCode)]

  watchdogProp . propertyOnce $ forM_ supplyValues $ \(totalSupply, delegatedSupply, expectedExitCode) ->
    moduleWorkspace "tmp" $ \tempDir -> do
      let outputDir = tempDir </> "out"

      (exitCode, _stdout, stderr) <-
        H.noteShowM $
          execDetailCardanoCLI
            [ "conway"
            , "genesis"
            , "create-testnet-data"
            , "--testnet-magic"
            , "42"
            , "--pools"
            , "3"
            , "--total-supply"
            , show totalSupply
            , "--delegated-supply"
            , show delegatedSupply
            , "--stake-delegators"
            , "3"
            , "--utxo-keys"
            , "3"
            , "--drep-keys"
            , "3"
            , "--out-dir"
            , outputDir
            ]

      H.note_ "check that exit code is equal to the expected one"
      exitCode === expectedExitCode

      if exitCode == ExitSuccess
        then do
          testGenesis@TestGenesis{maxLovelaceSupply, initialFunds} <-
            H.leftFailM . H.readJsonFile $ outputDir </> "shelley-genesis.json"
          H.note_ $ show testGenesis

          H.note_ "check that max lovelace supply is set equal to --total-supply flag value"
          maxLovelaceSupply === totalSupply

          H.note_ "check that all initial funds are positive"
          H.assertWith initialFunds $ all (>= 0) . M.elems

          H.note_ "check that initial funds are not bigger than max lovelace supply"
          H.assertWith initialFunds $ \initialFunds' -> do
            let totalDistributed = sum . M.elems $ initialFunds'
            totalDistributed <= maxLovelaceSupply
        else do
          H.assertWith stderr (`contains` "delegated supply should be less or equal to the total supply")
 where
  contains s1 s2 = s2 `isInfixOf` s1

data TestGenesis = TestGenesis
  { maxLovelaceSupply :: Int
  , initialFunds :: Map Text Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | This test tests the transient case, i.e. it writes strictly
-- less things to disk than 'hprop_golden_create_testnet_data'. Execute this test with:
-- @cabal test cardano-cli-test --test-options '-p "/create testnet data transient stake delegators/'@
hprop_create_testnet_data_transient_stake_delegators :: Property
hprop_create_testnet_data_transient_stake_delegators =
  watchdogProp . propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
    let outputDir = tempDir </> "out"

    void $
      execCardanoCLI
        [ "conway"
        , "genesis"
        , "create-testnet-data"
        , "--genesis-keys"
        , "2"
        , "--utxo-keys"
        , "3"
        , "--out-dir"
        , outputDir
        , "--testnet-magic"
        , "623"
        , "--pools"
        , "2"
        , "--transient-drep-keys"
        , "5"
        , "--transient-stake-delegators"
        , "4"
        ]

    H.note_ "check that DRep key folder was not created"
    assertDirectoryMissing (outputDir </> "drep-keys")

    H.note_ "check that stake delegator key folder was not created"
    assertDirectoryMissing (outputDir </> "stake-delegators")

-- For the golden part of this test, we are anyway covered by 'hprop_golden_create_testnet_data'
-- that generates strictly more stuff.
