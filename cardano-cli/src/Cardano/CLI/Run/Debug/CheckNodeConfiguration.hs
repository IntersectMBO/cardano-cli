{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Run.Debug.CheckNodeConfiguration (runCheckNodeConfig) where

import           Cardano.Api

import qualified Cardano.Chain.Genesis as Byron
import           Cardano.CLI.Commands.Debug.CheckNodeConfiguration
import           Cardano.CLI.Types.Errors.DebugCmdError
import qualified Cardano.Crypto.Hash as Crypto

import           Control.Monad
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import           System.FilePath (takeDirectory, (</>))

runCheckNodeConfig :: CheckNodeConfigCmdArgs -> ExceptT DebugCmdError IO ()
runCheckNodeConfig (CheckNodeConfigCmdArgs configFile) = do
  nodeConfig :: NodeConfig <- liftIO $ Yaml.decodeFileThrow configFilePath
  checkNodeGenesisConfiguration configFile nodeConfig
  liftIO $ putStrLn $ "Successfully checked node configuration file: " <> configFilePath
 where
  configFilePath = unFile configFile

-- | Build a 'CheckGenesisAnalysis' value from the given node configuration file, for the given era.
-- Reads the genesis file path and hash from the node configuration file, if they exist.
checkNodeGenesisConfiguration
  :: NodeConfigFile 'In
  -- ^ The node configuration file path. It's not read by this function, but used for producing error messages.
  -> NodeConfig
  -- ^ The parsed node configuration file
  -- ^ The era whose data must be analyzed. This parameter isn't about the eras we support.
  -> ExceptT DebugCmdError IO ()
checkNodeGenesisConfiguration configFile nodeConfig = do
  let configFilePath = unFile configFile
      byronGenFile = unFile $ ncByronGenesisFile nodeConfig
      _shelleyGenFile = takeDirectory configFilePath </> unFile (ncShelleyGenesisFile nodeConfig)
      _alonzoGenFile = takeDirectory configFilePath </> unFile (ncAlonzoGenesisFile nodeConfig)
      _conwayGenFile =
        takeDirectory configFilePath
          </> maybe (error "mkAnalysis: TODO add error constructor") unFile (ncConwayGenesisFile nodeConfig)

      byronGenHash = unGenesisHashByron $ ncByronGenesisHash nodeConfig
      _shelleyGenHash = Crypto.hashToTextAsHex $ unGenesisHashShelley $ ncShelleyGenesisHash nodeConfig
      _alonzoGenHash = Crypto.hashToTextAsHex $ unGenesisHashAlonzo $ ncAlonzoGenesisHash nodeConfig

  -- Byron genesis checks
  (_, Byron.GenesisHash byronHash) <-
    firstExceptT (DebugNodeConfigGenesisDataCmdError byronGenFile) $
      Byron.readGenesisData byronGenFile
  -- NB: I haven't looked to see if this is the appropriate way to render the Byron hash.
  let byronGenHashAsText = Text.pack $ show byronHash

  -- NB: The config file path will be enough to differentiate which genesis we are talking about.
  -- We don't need AnyCardanoEra in DebugNodeConfigWrongGenesisHashCmdError
  when (byronGenHash /= byronGenHashAsText) $
    throwError $
      DebugNodeConfigWrongGenesisHashCmdError
        (unFile configFile)
        (AnyCardanoEra ByronEra)
        byronGenHashAsText
        byronGenHash

-- Shelley genesis checks
-- ...
-- Alonzo genesis checks
-- ...
-- Conway genesis checks
-- ...
