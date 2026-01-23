{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraIndependent.Debug.CheckNodeConfiguration.Run (runCheckNodeConfig) where

import Cardano.Api
import Cardano.Api.Byron qualified as Byron

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraIndependent.Debug.CheckNodeConfiguration.Command
import Cardano.CLI.Read qualified as Read
import Cardano.CLI.Type.Error.DebugCmdError
import Cardano.Crypto.Hash qualified as Crypto

import Data.Text qualified as Text
import Data.Yaml qualified as Yaml
import System.FilePath (takeDirectory, (</>))

runCheckNodeConfig :: CheckNodeConfigCmdArgs -> CIO e ()
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
  -> CIO e ()
checkNodeGenesisConfiguration configFile nodeConfig = do
  let byronGenFile = adjustFilepath $ unFile $ ncByronGenesisFile nodeConfig
      alonzoGenFile = adjustFilepath $ unFile $ ncAlonzoGenesisFile nodeConfig
      shelleyGenFile = adjustFilepath $ unFile $ ncShelleyGenesisFile nodeConfig
      conwayGenFile = adjustFilepath $ unFile $ ncConwayGenesisFile nodeConfig

  liftIO $ putStrLn $ "Checking byron genesis file: " <> byronGenFile

  let mExpectedByronHash = unGenesisHashByron <$> ncByronGenesisHash nodeConfig
      mExpectedAlonzoHash = Crypto.hashToTextAsHex . unGenesisHashAlonzo <$> ncAlonzoGenesisHash nodeConfig
      mExpectedShelleyHash = Crypto.hashToTextAsHex . unGenesisHashShelley <$> ncShelleyGenesisHash nodeConfig
      mExpectedConwayHash = Crypto.hashToTextAsHex . unGenesisHashConway <$> ncConwayGenesisHash nodeConfig

  (_, Byron.GenesisHash byronHash) <-
    fromExceptTCli $
      Byron.readGenesisData byronGenFile
  let actualByronHash = Text.pack $ show byronHash
  actualAlonzoHash <- Crypto.hashToTextAsHex <$> Read.readShelleyOnwardsGenesisAndHash alonzoGenFile
  actualShelleyHash <- Crypto.hashToTextAsHex <$> Read.readShelleyOnwardsGenesisAndHash shelleyGenFile
  actualConwayHash <- Crypto.hashToTextAsHex <$> Read.readShelleyOnwardsGenesisAndHash conwayGenFile

  let
  checkHashIfPresent byronGenFile actualByronHash mExpectedByronHash
  checkHashIfPresent alonzoGenFile actualAlonzoHash mExpectedAlonzoHash
  checkHashIfPresent shelleyGenFile actualShelleyHash mExpectedShelleyHash
  checkHashIfPresent conwayGenFile actualConwayHash mExpectedConwayHash
 where
  ifJustAndDifferent :: Eq a => a -> (a -> CIO e ()) -> Maybe a -> CIO e ()
  ifJustAndDifferent actual f (Just expected)
    | expected /= actual = f expected
  ifJustAndDifferent _ _ _ = pure ()

  checkHashIfPresent :: FilePath -> Text.Text -> Maybe Text.Text -> CIO e ()
  checkHashIfPresent fp actual =
    ifJustAndDifferent
      actual
      ( throwCliError
          . DebugNodeConfigWrongGenesisHashCmdError
            configFilePath
            fp
            actual
      )

  configFilePath = unFile configFile
  -- We make the genesis filepath relative to the node configuration file, like the node does:
  -- https://github.com/IntersectMBO/cardano-node/blob/9671e7b6a1b91f5a530722937949b86deafaad43/cardano-node/src/Cardano/Node/Configuration/POM.hs#L668
  -- Note that, if the genesis filepath is absolute, the node configuration file's directory is ignored (by property of </>)
  adjustFilepath f = takeDirectory configFilePath </> f
