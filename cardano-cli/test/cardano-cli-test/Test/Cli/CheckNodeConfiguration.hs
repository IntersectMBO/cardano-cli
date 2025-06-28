{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cli.CheckNodeConfiguration where

import Cardano.Api

import Control.Monad (forM_)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.List (isInfixOf)
import Data.Text qualified as Text
import Data.Yaml qualified as Yaml
import GHC.IO.Exception (ExitCode (..))
import System.FilePath ((</>))

import Test.Cardano.CLI.Util (execCardanoCLI, execDetailCardanoCLI, watchdogProp)
import Test.Cardano.CLI.Workspace

import Hedgehog (Property)
import Hedgehog qualified as H
import Hedgehog.Extras (propertyOnce)
import Hedgehog.Extras qualified as H

nodeConfigFile :: FilePath
nodeConfigFile = "test/cardano-cli-test/files/input/check-node-configuration/node-config.json"

-- Execute this test with:
-- @cabal test cardano-cli-test --test-options '-p "/check node configuration success/"'@
hprop_check_node_configuration_success :: Property
hprop_check_node_configuration_success =
  watchdogProp . propertyOnce $ do
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
  deriving (Bounded, Enum)

-- Execute this test with:
-- @cabal test cardano-cli-test --test-options '-p "/check node configuration failure/"'@
hprop_check_node_configuration_failure :: Property
hprop_check_node_configuration_failure = do
  let supplyValues :: [(FilePath, FiddleKind)] =
        [ (path, fiddleKind)
        | path <-
            [ "test/cardano-cli-test/files/input/check-node-configuration/node-config.json"
            , "test/cardano-cli-test/files/input/check-node-configuration/node-config.yaml"
            ]
        , fiddleKind <- [minBound .. maxBound]
        ]

  propertyOnce $ forM_ supplyValues $ \(nodeConfigPath, fiddleKind) -> moduleWorkspace2 "tmp" $ \tempDir -> do
    let finalInputConfig = tempDir </> "node-config-changed.json"
        -- TODO why is that writing to "AlonzoGenesisHash" writes one more 0 than specified here? (and hence the need for this hack)
        wrongHash = Text.pack $ replicate (case fiddleKind of FiddleByron -> 65; FiddleNonByron -> 64) '0'

    -- Install the genesis files in the sandbox
    forM_
      [AnyCardanoEra ByronEra, AnyCardanoEra AlonzoEra, AnyCardanoEra ConwayEra, AnyCardanoEra ShelleyEra]
      $ \(AnyCardanoEra era) -> do
        let filename = Text.unpack $ "genesis." <> Text.toLower (eraToStringKey era) <> ".spec.json"
            genesisFile = "test/cardano-cli-test/files/input/check-node-configuration" </> filename
        H.copyFile genesisFile (tempDir </> filename)

    -- We make a hash value incorrect, and check that
    -- check-node-configuration fails.

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

    -- Prepare file with incorrect hash

    let fiddledEraKey =
          Aeson.fromText $ eraToGenesisHashKey $ case fiddleKind of
            FiddleByron -> AnyCardanoEra ByronEra
            FiddleNonByron -> AnyCardanoEra AlonzoEra
        finalConfigObject =
          Aeson.Object $ Aeson.insert fiddledEraKey (Aeson.String wrongHash) nodeConfigObject

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

    H.assertWith exitCode (ExitSuccess /=)
    H.assertWith stderr ("Wrong genesis hash" `isInfixOf`)

-- | The JSON key of the genesis hash for the given era.
eraToGenesisHashKey :: AnyCardanoEra -> Text.Text
eraToGenesisHashKey (AnyCardanoEra era) = eraToStringKey era <> "GenesisHash"

-- | Part of the JSON keys associated with the given era. For example,
-- @eraToStringKey ByronEra@ returns @"Byron"@.
eraToStringKey :: CardanoEra a -> Text.Text
eraToStringKey = docToText . pretty
