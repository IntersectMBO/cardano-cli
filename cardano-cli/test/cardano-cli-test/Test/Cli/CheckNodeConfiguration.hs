{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cli.CheckNodeConfiguration where

import Cardano.Api

import Control.Monad (forM_)
import Control.Monad.Catch (MonadCatch)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.List (isInfixOf)
import Data.Text qualified as Text
import Data.Yaml qualified as Yaml
import GHC.IO.Exception (ExitCode (..))
import GHC.Stack (HasCallStack)
import System.FilePath ((</>))

import Test.Cardano.CLI.Util (execCardanoCLI, execDetailCardanoCLI, watchdogProp)

import Hedgehog (MonadTest, Property)
import Hedgehog qualified as H
import Hedgehog.Extras (propertyOnce)
import Hedgehog.Extras qualified as H

-- | The directory with the node configurations and genesis files of these tests.
inputDir :: FilePath
inputDir = "test/cardano-cli-test/files/input/check-node-configuration"

-- | The hash of @genesis.alonzo.spec.json@.
alonzoGenesisHash :: Text.Text
alonzoGenesisHash = "2418fe881f57d2f5851a018214634aa6d1b7f10f130c3bdf8c13210c0ed2fd02"

-- | The hash of @genesis.dijkstra.spec.json@.
dijkstraGenesisHash :: Text.Text
dijkstraGenesisHash = "448c52809fdd686b4085adb78668b3708ba5b83846933f7f8f38ac1aa12ebcf6"

-- Execute this test with:
-- @cabal test cardano-cli-test --test-options '-p "/check node configuration success/"'@
hprop_check_node_configuration_success :: Property
hprop_check_node_configuration_success =
  watchdogProp . propertyOnce $ do
    -- node-config-envelope.json uses the same genesis files, in the new cardano-config format
    forM_ ["node-config.json", "node-config.yaml", "node-config-envelope.json"] $ \nodeConfigFile ->
      H.noteM_ $
        execCardanoCLI
          [ "debug"
          , "check-node-configuration"
          , "--node-configuration-file"
          , inputDir </> nodeConfigFile
          ]

data FiddleKind
  = FiddleByron
  | FiddleNonByron
  deriving (Bounded, Enum)

-- Execute this test with:
-- @cabal test cardano-cli-test --test-options '-p "/check node configuration failure/"'@
hprop_check_node_configuration_failure :: Property
hprop_check_node_configuration_failure = do
  let supplyValues :: [(FilePath, FiddleKind)] =
        [ (path, fiddleKind)
        | path <-
            [ inputDir </> "node-config.json"
            , inputDir </> "node-config.yaml"
            ]
        , fiddleKind <- [minBound .. maxBound]
        ]

  watchdogProp . propertyOnce $ forM_ supplyValues $ \(nodeConfigPath, fiddleKind) -> H.moduleWorkspace "tmp" $ \tempDir -> do
    -- TODO why is that writing to "AlonzoGenesisHash" writes one more 0 than specified here? (and hence the need for this hack)
    let wrongHash = Text.pack $ replicate (case fiddleKind of FiddleByron -> 65; FiddleNonByron -> 64) '0'
        fiddledEraKey =
          Aeson.fromText $ eraToGenesisHashKey $ case fiddleKind of
            FiddleByron -> AnyCardanoEra ByronEra
            FiddleNonByron -> AnyCardanoEra AlonzoEra

    -- We make a hash value incorrect, and check that
    -- check-node-configuration finds the mistake.
    finalInputConfig <-
      writeChangedNodeConfig tempDir nodeConfigPath $
        Aeson.insert fiddledEraKey (Aeson.String wrongHash)

    stderr <- checkNodeConfigurationFails finalInputConfig
    -- The 65-character Byron hash is not a hash at all, so the configuration is
    -- at fault. The Alonzo hash is a well-formed hash of something else, so the
    -- Alonzo genesis file is.
    forM_
      ( case fiddleKind of
          FiddleByron -> ["Error reading node configuration", "ByronGenesisHash"]
          FiddleNonByron ->
            [ "Error reading the genesis file"
            , "genesis.alonzo.spec.json"
            , "AlonzoGenesisFile"
            , Text.unpack alonzoGenesisHash
            , Text.unpack wrongHash
            ]
      )
      $ \expected -> H.assertWith stderr (expected `isInfixOf`)

-- Execute this test with:
-- @cabal test cardano-cli-test --test-options '-p "/check node configuration missing hash/"'@
hprop_check_node_configuration_missing_hash :: Property
hprop_check_node_configuration_missing_hash =
  watchdogProp . propertyOnce $ do
    let eras =
          [AnyCardanoEra ByronEra, AnyCardanoEra ShelleyEra, AnyCardanoEra AlonzoEra, AnyCardanoEra ConwayEra]
    forM_ eras $ \era -> H.moduleWorkspace "tmp" $ \tempDir -> do
      let hashKey = eraToGenesisHashKey era

      -- We remove the hash of one genesis file
      finalInputConfig <-
        writeChangedNodeConfig tempDir (inputDir </> "node-config.json") $
          Aeson.delete (Aeson.fromText hashKey)

      stderr <- checkNodeConfigurationFails finalInputConfig
      H.assertWith stderr (Text.unpack hashKey `isInfixOf`)

-- Execute this test with:
-- @cabal test cardano-cli-test --test-options '-p "/check node configuration dijkstra/"'@
hprop_check_node_configuration_dijkstra :: Property
hprop_check_node_configuration_dijkstra =
  watchdogProp . propertyOnce $ do
    -- With ExperimentalHardForksEnabled, the Dijkstra genesis is read too
    let enableDijkstra =
          Aeson.insert "ExperimentalHardForksEnabled" (Aeson.Bool True)
        withDijkstraGenesis hash =
          Aeson.insert "DijkstraGenesisFile" "genesis.dijkstra.spec.json"
            . Aeson.insert "DijkstraGenesisHash" (Aeson.String hash)
        wrongHash = Text.pack $ replicate 64 '0'

    -- The right hash
    H.moduleWorkspace "tmp" $ \tempDir -> do
      finalInputConfig <-
        writeChangedNodeConfig tempDir (inputDir </> "node-config.json") $
          enableDijkstra . withDijkstraGenesis dijkstraGenesisHash
      H.noteM_ $
        execCardanoCLI
          [ "debug"
          , "check-node-configuration"
          , "--node-configuration-file"
          , finalInputConfig
          ]

    -- A wrong hash
    H.moduleWorkspace "tmp" $ \tempDir -> do
      finalInputConfig <-
        writeChangedNodeConfig tempDir (inputDir </> "node-config.json") $
          enableDijkstra . withDijkstraGenesis wrongHash
      stderr <- checkNodeConfigurationFails finalInputConfig
      forM_ ["genesis.dijkstra.spec.json", dijkstraGenesisHash, wrongHash] $ \expected ->
        H.assertWith stderr (Text.unpack expected `isInfixOf`)

    -- No Dijkstra genesis
    H.moduleWorkspace "tmp" $ \tempDir -> do
      finalInputConfig <-
        writeChangedNodeConfig tempDir (inputDir </> "node-config.json") enableDijkstra
      stderr <- checkNodeConfigurationFails finalInputConfig
      H.assertWith stderr ("DijkstraGenesisFile" `isInfixOf`)

-- | Copy the genesis files to the given directory, and write there the given
-- node configuration, changed by the given function. Returns the path of the
-- changed node configuration.
writeChangedNodeConfig
  :: (HasCallStack, MonadIO m, MonadTest m)
  => FilePath
  -- ^ The directory to write to
  -> FilePath
  -- ^ The node configuration to change (JSON or YAML)
  -> (Aeson.Object -> Aeson.Object)
  -- ^ The change
  -> m FilePath
writeChangedNodeConfig tempDir nodeConfigPath change = do
  let finalInputConfig = tempDir </> "node-config-changed.json"

  -- Install the genesis files in the sandbox
  forM_ ["byron", "shelley", "alonzo", "conway", "dijkstra"] $ \era -> do
    let filename = "genesis." <> era <> ".spec.json"
    H.copyFile (inputDir </> filename) (tempDir </> filename)

  nodeConfigValue :: Aeson.Value <- Yaml.decodeFileThrow nodeConfigPath
  nodeConfigObject :: Aeson.Object <-
    case nodeConfigValue of
      Aeson.Object obj -> pure obj
      _ ->
        do
          H.note_ "Expected an Object, but got something else"
          H.failure

  let finalConfigObject = Aeson.Object $ change nodeConfigObject
  liftIO $ LBS.writeFile finalInputConfig $ Aeson.encodePretty finalConfigObject
  pure finalInputConfig

-- | Run check-node-configuration on the given node configuration, check that it
-- fails, and return its stderr.
checkNodeConfigurationFails
  :: (HasCallStack, MonadIO m, MonadTest m, MonadCatch m)
  => FilePath
  -> m String
checkNodeConfigurationFails nodeConfigFile = do
  (exitCode, _stdout, stderr) <-
    H.noteShowM $
      execDetailCardanoCLI
        [ "debug"
        , "check-node-configuration"
        , "--node-configuration-file"
        , nodeConfigFile
        ]
  H.assertWith exitCode (ExitSuccess /=)
  pure stderr

-- | The JSON key of the genesis hash for the given era.
eraToGenesisHashKey :: AnyCardanoEra -> Text.Text
eraToGenesisHashKey (AnyCardanoEra era) = eraToStringKey era <> "GenesisHash"

-- | Part of the JSON keys associated with the given era. For example,
-- @eraToStringKey ByronEra@ returns @"Byron"@.
eraToStringKey :: CardanoEra a -> Text.Text
eraToStringKey = docToText . pretty
