{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cli.CheckNodeConfiguration where

import           Cardano.Api

import           Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.List (isInfixOf)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import           GHC.IO.Exception (ExitCode (..))
import           System.FilePath ((</>))

import           Test.Cardano.CLI.Util (FileSem, bracketSem, execCardanoCLI, execDetailCardanoCLI,
                   newFileSem)

import           Hedgehog (Property)
import qualified Hedgehog as H
import           Hedgehog.Extras (propertyOnce)
import qualified Hedgehog.Extras as H

-- | Semaphore protecting against locked file error, when running properties concurrently.
nodeConfigSem :: FileSem
nodeConfigSem = newFileSem "test/cardano-cli-test/files/input/check-node-configuration/node-config.json"
{-# NOINLINE nodeConfigSem #-}

-- Execute this test with:
-- @cabal test cardano-cli-test --test-options '-p "/check node configuration success/"'@
hprop_check_node_configuration_success :: Property
hprop_check_node_configuration_success =
  propertyOnce $ do
    -- We test that the command doesn't crash, because otherwise
    -- execCardanoCLI would fail.
    bracketSem nodeConfigSem $ \nodeConfigFile ->
      H.noteM_ $
        execCardanoCLI
          [ "debug"
          , "check-node-configuration"
          , "--node-configuration-file"
          , nodeConfigFile
          ]

data FiddleKind
  = FiddleByron
  | FiddleNonByron
  | RemoveAll
  deriving (Bounded, Enum)

-- Execute this test with:
-- @cabal test cardano-cli-test --test-options '-p "/check node configuration fiddle/"'@
hprop_check_node_configuration_fiddle :: Property
hprop_check_node_configuration_fiddle = do
  let supplyValues :: [(FilePath, FiddleKind)] =
        [ (path, fiddleKind)
        | path <-
            [ "test/cardano-cli-test/files/input/check-node-configuration/node-config.json"
            , "test/cardano-cli-test/files/input/check-node-configuration/node-config.yaml"
            ]
        , fiddleKind <- [minBound .. maxBound]
        ]

  propertyOnce $ forM_ supplyValues $ \(nodeConfigPath, fiddleKind) -> H.moduleWorkspace "tmp" $ \tempDir ->
    bracketSem nodeConfigSem $ \_ -> do
      let finalInputConfig = tempDir </> "node-config-changed.json"
          wrongHash = Text.pack $ replicate 65 '0'

      -- Install the genesis files in the sandbox
      forM_
        [AnyCardanoEra ByronEra, AnyCardanoEra AlonzoEra, AnyCardanoEra ConwayEra, AnyCardanoEra ShelleyEra]
        $ \era -> do
          let filename = Text.unpack $ "genesis." <> Text.toLower (eraToStringKey era) <> ".spec.json"
              genesisFile = "test/cardano-cli-test/files/input/check-node-configuration" </> filename
          H.copyFile genesisFile (tempDir </> filename)

      -- We make a hash value incorrect, and check that
      -- check-node-configuration finds the mistake.
      --
      -- Then we call with --fix, and check that the command goes through

      nodeConfigValue :: Aeson.Value <- Yaml.decodeFileThrow nodeConfigPath
      nodeConfigObject :: Aeson.Object <-
        case nodeConfigValue of
          Aeson.Object obj -> pure obj
          _ ->
            do
              H.note_ "Expected an Object, but got something else"
              H.failure

      -- We make a hash value incorrect, and check that
      -- check-node-configuration finds the mistake.
      --
      -- Then we call with --fix, and check that the command goes through
      -- when calling it again

      -- Prepare file with incorrect hash

      let finalConfigObject =
            Aeson.Object $ case fiddleKind of
              FiddleByron -> do
                Aeson.insert "ByronGenesisHash" (Aeson.String wrongHash) nodeConfigObject
              FiddleNonByron ->
                Aeson.insert "AlonzoGenesisHash" (Aeson.String wrongHash) nodeConfigObject
              RemoveAll ->
                removeKeys (map (Aeson.fromText . eraToStringKey) eras) nodeConfigObject

      -- Write file with incorrect hash
      liftIO $ LBS.writeFile finalInputConfig $ Aeson.encodePretty finalConfigObject

      (exitCode, _stdout, stderr) <-
        H.noteShowM $
          execDetailCardanoCLI
            [ "debug"
            , "check-node-configuration"
            , "--node-configuration-file"
            , finalInputConfig
            ]

      case fiddleKind of
        RemoveAll ->
          -- We removed all hashes, so there's no error reported
          pure ()
        _ -> do
          H.assertWith exitCode (ExitSuccess /=)
          H.assertWith stderr ("Wrong genesis hash" `isInfixOf`)

      -- Fix the hashes
      H.noteM_ $
        execCardanoCLI
          [ "debug"
          , "check-node-configuration"
          , "--node-configuration-file"
          , finalInputConfig
          , "--fix"
          ]

      -- Now call without --fix, to check that the command goes through
      H.noteM_ $
        execCardanoCLI
          [ "debug"
          , "check-node-configuration"
          , "--node-configuration-file"
          , finalInputConfig
          ]

      -- Finally check that the fixed file is the same as the original

      reloadedNodeConfigValue :: Aeson.Value <- H.readJsonFileOk finalInputConfig

      reloadedNodeConfigValue H.=== nodeConfigValue
 where
  eras :: [AnyCardanoEra] = [minBound .. maxBound]
  removeKeys :: [Aeson.Key] -> Aeson.Object -> Aeson.Object
  removeKeys keys kv = foldr Aeson.delete kv keys

-- | Part of the JSON keys associated with the given era.
eraToStringKey :: AnyCardanoEra -> Text.Text
eraToStringKey (AnyCardanoEra era) =
  case era of
    ByronEra -> "Byron"
    ShelleyEra -> "Shelley"
    AllegraEra -> "Allegra"
    MaryEra -> "Mary"
    AlonzoEra -> "Alonzo"
    BabbageEra -> "Babbage"
    ConwayEra -> "Conway"
