{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cli.CreateTestnetData where

import           Control.Monad (forM_, void)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.List (isInfixOf)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           GHC.IO.Exception (ExitCode (..))
import           System.FilePath ((</>))

import           Test.Cardano.CLI.Util (assertDirectoryMissing, execCardanoCLI,
                   execDetailCardanoCLI)

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

  propertyOnce $ forM_ supplyValues $ \(totalSupply, delegatedSupply, expectedExitCode) ->
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
  deriving (Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

-- | This test tests the transient case, i.e. it writes strictly
-- less things to disk than 'hprop_golden_create_testnet_data'. Execute this test with:
-- @cabal test cardano-cli-test --test-options '-p "/create testnet data transient stake delegators/'@
hprop_create_testnet_data_transient_stake_delegators :: Property
hprop_create_testnet_data_transient_stake_delegators =
  propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
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

-- Execute this test with:
-- @cabal test cardano-cli-test --test-options '-p "/create testnet wrong genesis hash/"'@
hprop_create_testnet_wrong_genesis_hash :: Property
hprop_create_testnet_wrong_genesis_hash =
  propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
    let outputDir = tempDir </> "out"

    (exitCode, _stdout, stderr) <-
      H.noteShowM $
        execDetailCardanoCLI
          [ "conway"
          , "genesis"
          , "create-testnet-data"
          , "--testnet-magic"
          , "42"
          , "--node-configuration"
          , "test/cardano-cli-test/files/input/conway/create-testnet-data/node-config.json"
          , "--out-dir"
          , outputDir
          ]

    exitCode === ExitFailure 1
    H.assertWith stderr ("Hash associated to key \"ConwayGenesisHash\" in file" `isInfixOf`)

-- Execute this test with:
-- @cabal test cardano-cli-test --test-options '-p "/create testnet creates correct hashes and paths/"'@
hprop_create_testnet_creates_correct_hashes_and_paths :: Property
hprop_create_testnet_creates_correct_hashes_and_paths =
  propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
    let outputDir = tempDir </> "out"
        outputDir2 = tempDir </> "out2"
        configFile = "test/cardano-cli-test/files/input/conway/create-testnet-data/node-config.json"
        smallerConfigFile = tempDir </> "smaller-config.json"
        eras = ["Alonzo", "Conway", "Shelley"]
        keys = concat [[era <> "GenesisHash", era <> "GenesisFile"] | era <- eras]

    -- Copy and modify the node-config.json file
    originalConfig <- H.readJsonFileOk configFile
    let modifiedConfig = removeKeys keys originalConfig
    liftIO $ LBS.writeFile smallerConfigFile $ Aeson.encodePretty modifiedConfig

    -- Execute create-testnet-data with the small configuration. This will make create-testnet-data
    -- augment the configuration file with the hashes and paths.
    H.noteShowM_ $
      execCardanoCLI
        [ "conway"
        , "genesis"
        , "create-testnet-data"
        , "--testnet-magic"
        , "42"
        , "--node-configuration"
        , smallerConfigFile
        , "--out-dir"
        , outputDir
        ]

    let augmentedConfigPath = outputDir </> "configuration.json"
    augmentedConfig :: Aeson.Value <- H.readJsonFileOk augmentedConfigPath
    H.assertWith augmentedConfig $ \config -> all (hasKey config) keys

    -- Execute creates-testnet-data again with the augmented configuration file
    -- It should not fail, meaning the hashes and paths generated by the previous call are correct.
    -- But we need to remove the ShelleyGenesisHash key first, because the content of shelley genesis file
    -- is not static; because it contains hashes of delegated keys.

    let shelleyLessAugmentedConfigPath = outputDir </> "configuration.json"
    liftIO $
      LBS.writeFile shelleyLessAugmentedConfigPath $
        Aeson.encodePretty $
          removeKeys ["ShelleyGenesisHash"] augmentedConfig

    H.noteShowM_ $
      execCardanoCLI
        [ "conway"
        , "genesis"
        , "create-testnet-data"
        , "--testnet-magic"
        , "42"
        , "--node-configuration"
        , shelleyLessAugmentedConfigPath
        , "--out-dir"
        , outputDir2
        ]
 where
  removeKeys :: [Text] -> Aeson.Value -> Aeson.Value
  removeKeys keys =
    \case
      Aeson.Object obj -> Aeson.Object $ foldr (Aeson.delete . Aeson.fromText) obj keys
      _ -> error "Invalid JSON content: expected an Object"
  hasKey :: Aeson.Value -> Text -> Bool
  hasKey v key =
    case v of
      Aeson.Object obj -> Aeson.member (Aeson.fromText key) obj
      _ -> False
