{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Run.Debug.CheckNodeConfiguration (runCheckNodeConfig) where

import           Cardano.Api
import qualified Cardano.Api.Byron as Byron

import           Cardano.CLI.Commands.Debug.CheckNodeConfiguration
import qualified Cardano.CLI.Read as Read
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

checkNodeGenesisConfiguration
  :: NodeConfigFile 'In
  -- ^ The node configuration file path. It's not read by this function, but used for producing error messages.
  -> NodeConfig
  -- ^ The parsed node configuration file
  -> ExceptT DebugCmdError IO ()
checkNodeGenesisConfiguration configFile nodeConfig = do
  let byronGenFile = adjustFilepath $ unFile $ ncByronGenesisFile nodeConfig
      alonzoGenFile = adjustFilepath $ unFile $ ncAlonzoGenesisFile nodeConfig
      shelleyGenFile = adjustFilepath $ unFile $ ncShelleyGenesisFile nodeConfig
  conwayGenFile <- case ncConwayGenesisFile nodeConfig of
    Nothing -> throwError $ DebugNodeConfigNoConwayFileCmdError configFilePath
    Just conwayGenesisFile -> pure $ adjustFilepath $ unFile conwayGenesisFile

  liftIO $ putStrLn $ "Checking byron genesis file: " <> byronGenFile

  let expectedByronHash = unGenesisHashByron $ ncByronGenesisHash nodeConfig
      expectedAlonzoHash = Crypto.hashToTextAsHex $ unGenesisHashAlonzo $ ncAlonzoGenesisHash nodeConfig
      expectedShelleyHash = Crypto.hashToTextAsHex $ unGenesisHashShelley $ ncShelleyGenesisHash nodeConfig
  expectedConwayHash <- case ncConwayGenesisHash nodeConfig of
    Nothing -> throwError $ DebugNodeConfigNoConwayHashCmdError configFilePath
    Just conwayGenesisHash -> pure $ Crypto.hashToTextAsHex $ unGenesisHashConway conwayGenesisHash

  (_, Byron.GenesisHash byronHash) <-
    firstExceptT (DebugNodeConfigGenesisDataCmdError byronGenFile) $
      Byron.readGenesisData byronGenFile
  let actualByronHash = Text.pack $ show byronHash
  actualAlonzoHash <- Crypto.hashToTextAsHex <$> Read.readShelleyOnwardsGenesisAndHash alonzoGenFile
  actualShelleyHash <- Crypto.hashToTextAsHex <$> Read.readShelleyOnwardsGenesisAndHash shelleyGenFile
  actualConwayHash <- Crypto.hashToTextAsHex <$> Read.readShelleyOnwardsGenesisAndHash conwayGenFile

  when (actualByronHash /= expectedByronHash) $
    throwError $
      DebugNodeConfigWrongGenesisHashCmdError
        configFilePath
        byronGenFile
        actualByronHash
        expectedByronHash
  when (actualAlonzoHash /= expectedAlonzoHash) $
    throwError $
      DebugNodeConfigWrongGenesisHashCmdError
        configFilePath
        alonzoGenFile
        actualAlonzoHash
        expectedAlonzoHash
  when (actualShelleyHash /= expectedShelleyHash) $
    throwError $
      DebugNodeConfigWrongGenesisHashCmdError
        configFilePath
        shelleyGenFile
        actualShelleyHash
        expectedShelleyHash
  when (actualConwayHash /= expectedConwayHash) $
    throwError $
      DebugNodeConfigWrongGenesisHashCmdError
        configFilePath
        conwayGenFile
        actualConwayHash
        expectedConwayHash
 where
  configFilePath = unFile configFile
  -- We make the genesis filepath relative to the node configuration file, like the node does:
  -- https://github.com/IntersectMBO/cardano-node/blob/9671e7b6a1b91f5a530722937949b86deafaad43/cardano-node/src/Cardano/Node/Configuration/POM.hs#L668
  -- Note that, if the genesis filepath is absolute, the node configuration file's directory is ignored (by property of </>)
  adjustFilepath f = takeDirectory configFilePath </> f
