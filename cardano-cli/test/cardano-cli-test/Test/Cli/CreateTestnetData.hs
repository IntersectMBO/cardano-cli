{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cli.CreateTestnetData where

import           Control.Monad
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           GHC.Generics
import           GHC.IO.Exception (ExitCode (..))
import           System.FilePath

import           Test.Cardano.CLI.Util (execCardanoCLI, execDetailCardanoCLI)

import           Hedgehog (Property, success, (===))
import           Hedgehog.Extras (moduleWorkspace, propertyOnce)
import qualified Hedgehog.Extras as H

-- | Test case for https://github.com/IntersectMBO/cardano-cli/issues/587
-- Execute this test with:
-- @cabal test cardano-cli-test --test-options '-p "/create testnet data minimal/"'@
hprop_create_testnet_data_minimal :: Property
hprop_create_testnet_data_minimal =
  propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do

    let outputDir = tempDir </> "out"

    -- We test that the command doesn't crash, because otherwise
    -- execCardanoCLI would fail.
    H.noteM_ $ execCardanoCLI
      ["conway",  "genesis", "create-testnet-data"
      , "--testnet-magic", "42"
      , "--out-dir", outputDir
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
        , (1_000_000_000, 1_100_000_001, ExitFailure 1)
        , (1_000_000_000, 2_000_000_000, ExitFailure 1)
        ] :: [(Int, Int, ExitCode)]

  propertyOnce $ forM_ supplyValues $ \(totalSupply, delegatedSupply, expectedExitCode) ->
    moduleWorkspace "tmp" $ \tempDir -> do
      let outputDir = tempDir </> "out"

      (exitCode, _, _) <- H.noteShowM $ execDetailCardanoCLI
        ["conway",  "genesis", "create-testnet-data"
        , "--testnet-magic", "42"
        , "--pools", "3"
        , "--total-supply", show totalSupply
        , "--delegated-supply", show delegatedSupply
        , "--stake-delegators", "3"
        , "--utxo-keys", "3"
        , "--drep-keys", "3"
        , "--out-dir", outputDir
        ]

      H.note_ "check that exit code is equal to the expected one"
      exitCode === expectedExitCode

      when (exitCode == ExitSuccess) $ do
        testGenesis@TestGenesis{maxLovelaceSupply, initialFunds} <- H.leftFailM . H.readJsonFile $ outputDir </> "genesis.json"
        H.note_ $ show testGenesis

        H.note_ "check that max lovelace supply is set equal to --total-supply flag value"
        maxLovelaceSupply === totalSupply

        H.note_ "check that all initial funds are positive"
        H.assertWith initialFunds $ all (>= 0) . M.elems

        H.note_ "check that initial funds are not bigger than max lovelace supply"
        H.assertWith initialFunds $ \initialFunds' -> do
          let totalDistributed = sum . M.elems $ initialFunds'
          totalDistributed <= maxLovelaceSupply

data TestGenesis = TestGenesis
  { maxLovelaceSupply :: Int
  , initialFunds :: Map Text Int
  } deriving (Show, Generic, ToJSON, FromJSON)