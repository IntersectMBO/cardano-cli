{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Run.Debug.CheckNodeConfiguration (runCheckNodeConfig) where

import           Cardano.Api

import           Cardano.Chain.Genesis (GenesisHash (..), readGenesisData)
import           Cardano.CLI.Commands.Debug.CheckNodeConfiguration
import qualified Cardano.CLI.Read as Read
import           Cardano.CLI.Types.Errors.DebugCmdError
import qualified Cardano.Crypto.Hash as Crypto

import           Data.Maybe (catMaybes)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import           System.FilePath (takeDirectory, (</>))

runCheckNodeConfig :: CheckNodeConfigCmdArgs -> ExceptT DebugCmdError IO ()
runCheckNodeConfig (CheckNodeConfigCmdArgs configFile) = do
  nodeConfig :: NodeConfig <- liftIO $ Yaml.decodeFileThrow configFilePath
  analyses <- catMaybes <$> mapM (mkAnalysis configFile nodeConfig) eras
  mapM_ (runAnalysis configFile) analyses
  liftIO $ putStrLn $ "Successfully checked node configuration file: " <> configFilePath
 where
  configFilePath = unFile configFile
  eras :: [EraWithGenesisFile] = [minBound .. maxBound]

data CheckGenesisAnalysis = CheckGenesisAnalysis
  { concernedEra :: EraWithGenesisFile
  , genesisFilepath :: FilePath
  -- ^ The path to the genesis file, as found in the node configuration file, if any
  , actualHash :: Text.Text
  -- ^ The actual hash of the genesis file, computed from the file's content
  , expectedHash :: Maybe Text.Text
  -- ^ The hash of the genesis file, as found in the node configuration file, if any.
  }

runAnalysis
  :: NodeConfigFile 'In
  -- ^ The node configuration file. It's not read by this function, but used for producing error messages.
  -> CheckGenesisAnalysis
  -> ExceptT DebugCmdError IO ()
  -- ^ The fixed node configuration file
runAnalysis configFile (CheckGenesisAnalysis{concernedEra, genesisFilepath = _, actualHash, expectedHash}) = do
  case expectedHash of
    Nothing ->
      -- Genesis hash is not declared in the node configuration file: we have nothing to check against
      pure ()
    Just expectedHash'
      | expectedHash' == actualHash ->
          -- Genesis hash is correct: no error
          pure ()
    Just expectedHash' ->
      -- Genesis hash is incorrect: report an error
      throwError $
        DebugNodeConfigWrongGenesisHashCmdError
          (unFile configFile)
          (toAnyCardanoEra concernedEra)
          actualHash
          expectedHash'

-- | Build a 'CheckGenesisAnalysis' value from the given node configuration file, for the given era.
-- Reads the genesis file path and hash from the node configuration file, if they exist.
mkAnalysis
  :: NodeConfigFile 'In
  -- ^ The node configuration file path. It's not read by this function, but used for producing error messages.
  -> NodeConfig
  -- ^ The parsed node configuration file
  -> EraWithGenesisFile
  -- ^ The era whose data must be analyzed. This parameter isn't about the eras we support.
  -> ExceptT DebugCmdError IO (Maybe CheckGenesisAnalysis)
mkAnalysis configFile nodeConfig era = do
  case (getGenesisFilePath nodeConfig era, getGenesisHash nodeConfig era) of
    (Nothing, Nothing) ->
      -- Neither the genesis file nor the genesis hash are declared in the node configuration file
      -- Note that this is expected for eras whose hash doesn't appear in the node configuration file,
      -- for example Mary.
      pure Nothing
    (Nothing, Just _) -> do
      -- The path to the genesis file is not given in the node configuration file,
      -- but a genesis hash is provided. Since we can't verify the latter,
      -- we write something to stdout, because this is weird.
      liftIO $
        putStrLn $
          "Genesis file for " <> show era <> " is not declared in " <> configFilePath
      liftIO $ putStrLn "So the hash's correctness cannot be checked."
      pure Nothing
    (Just filepath, expectedHash) -> do
      -- We make the genesis filepath relative to the node configuration file, like the node does:
      -- https://github.com/IntersectMBO/cardano-node/blob/9671e7b6a1b91f5a530722937949b86deafaad43/cardano-node/src/Cardano/Node/Configuration/POM.hs#L668
      -- Note that, if the genesis filepath is absolute, the node configuration file's directory is ignored (by property of </>)
      let genesisFilePath = takeDirectory configFilePath </> filepath
      actualHash <-
        case era of
          ByronGenesis -> do
            (_, GenesisHash byronHash) <-
              firstExceptT (DebugNodeConfigGenesisDataCmdError genesisFilePath) $ readGenesisData genesisFilePath
            return $ Text.pack $ show byronHash
          _ -> Crypto.hashToTextAsHex <$> Read.readShelleyOnwardsGenesisAndHash genesisFilePath
      case expectedHash of
        Nothing ->
          pure $ Just $ CheckGenesisAnalysis era genesisFilePath actualHash Nothing
        Just expectedHash' ->
          pure $ Just $ CheckGenesisAnalysis era genesisFilePath actualHash (Just expectedHash')
 where
  configFilePath = unFile configFile

data EraWithGenesisFile
  = ByronGenesis
  | ShelleyGenesis
  | AlonzoGenesis
  | ConwayGenesis
  deriving (Eq, Enum, Bounded)

instance Show EraWithGenesisFile where
  show = \case
    ByronGenesis -> "Byron"
    ShelleyGenesis -> "Shelley"
    AlonzoGenesis -> "Alonzo"
    ConwayGenesis -> "Conway"

toAnyCardanoEra :: EraWithGenesisFile -> AnyCardanoEra
toAnyCardanoEra = \case
  ByronGenesis -> AnyCardanoEra ByronEra
  ShelleyGenesis -> AnyCardanoEra ShelleyEra
  AlonzoGenesis -> AnyCardanoEra AlonzoEra
  ConwayGenesis -> AnyCardanoEra ConwayEra

-- | Get the hash of the genesis file, of the given era.
getGenesisHash :: NodeConfig -> EraWithGenesisFile -> Maybe Text.Text
getGenesisHash config = \case
  ByronGenesis -> Just $ unGenesisHashByron $ ncByronGenesisHash config
  ShelleyGenesis -> Just $ Crypto.hashToTextAsHex $ unGenesisHashShelley $ ncShelleyGenesisHash config
  AlonzoGenesis -> Just $ Crypto.hashToTextAsHex $ unGenesisHashAlonzo $ ncAlonzoGenesisHash config
  ConwayGenesis -> Crypto.hashToTextAsHex . unGenesisHashConway <$> ncConwayGenesisHash config

-- | Get the path to the genesis file, of the given era.
getGenesisFilePath :: NodeConfig -> EraWithGenesisFile -> Maybe FilePath
getGenesisFilePath config = \case
  ByronGenesis -> Just $ unFile $ ncByronGenesisFile config
  ShelleyGenesis -> Just $ unFile $ ncShelleyGenesisFile config
  AlonzoGenesis -> Just $ unFile $ ncAlonzoGenesisFile config
  ConwayGenesis -> unFile <$> ncConwayGenesisFile config
