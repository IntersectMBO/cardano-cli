{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cardano.CLI.Run.Debug.CheckNodeConfiguration (runCheckNodeConfig, eraToStringKey) where

import           Cardano.Api

import           Cardano.Chain.Genesis (GenesisHash (..), readGenesisData)
import           Cardano.CLI.Commands.Debug.CheckNodeConfiguration hiding (fixTheFile)
import qualified Cardano.CLI.Read as Read
import           Cardano.CLI.Types.Common (FixNodeConfigFile (..))
import           Cardano.CLI.Types.Errors.DebugCmdError
import qualified Cardano.Crypto.Hash as Crypto
import           Cardano.Crypto.Hash.Class
import           Cardano.Prelude (when)
import qualified Cardano.Prelude as LBS

import           Control.Monad (foldM)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.List (isSuffixOf)
import           Data.Maybe (catMaybes)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Yaml as Yaml
import           System.Directory (removeFile)
import           System.FilePath (takeDirectory, (</>))

runCheckNodeConfig :: CheckNodeConfigCmdArgs -> ExceptT DebugCmdError IO ()
runCheckNodeConfig (CheckNodeConfigCmdArgs configFile fixTheFile) = do
  -- Use Cardano.CLI.EraBased.Run.Genesis.writeGenesisHashesToNodeConfigFile ?
  -- and CreateTestnetData.addOrCheck?
  nodeConfigValue <- Yaml.decodeFileThrow configFilePath
  nodeConfig <-
    case nodeConfigValue of
      Aeson.Object obj -> pure obj
      _ -> throwE $ DebugNodeConfigReadCmdError configFilePath "Expected an Object, but got something else"
  analyses <- catMaybes <$> mapM (\era -> mkAnalysis era configFile nodeConfig) eras
  nodeConfig' <- foldM (\obj -> runAnalysis configFile obj fixTheFile) nodeConfig analyses
  when (nodeConfig' /= nodeConfig) $ do
    -- Write the fixed node configuration file
    -- Better to remove the file first, to avoid a file busy exception when writing
    liftIO $ removeFile configFilePath
    -- Cheap trick to try to preserve the input file's format
    let looksLikeJson = ".json" `isSuffixOf` configFilePath
    liftIO $
      if looksLikeJson
        then LBS.writeFile configFilePath $ Aeson.encodePretty nodeConfig'
        else Yaml.encodeFile configFilePath nodeConfig'
  liftIO $ putStrLn $ "Successfully checked node configuration file: " <> configFilePath
  return ()
 where
  configFilePath = unFile configFile
  eras :: [AnyCardanoEra] = [minBound .. maxBound]

data Analysis = Analysis
  { concernedEra :: AnyCardanoEra
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
  -> Aeson.Object
  -- ^ The node configuration file, as a JSON object
  -> FixNodeConfigFile
  -> Analysis
  -> ExceptT DebugCmdError IO Aeson.Object
  -- ^ Whether the node configuration file was fixed, and the fixed node configuration file
runAnalysis configFile configFileObj fixMode (Analysis{concernedEra = era@(AnyCardanoEra cEra), genesisFilepath = _, actualHash, expectedHash}) = do
  case (expectedHash, fixMode) of
    (Nothing, _) ->
      -- Genesis hash is not declared in the node configuration file: we have nothing to check against
      pure configFileObj
    (Just expectedHash', _)
      | expectedHash' == actualHash ->
          -- Genesis hash is correct: no error and nothing to change
          pure configFileObj
    (Just expectedHash', DontFix) ->
      -- Genesis hash is incorrect and we don't want to fix it, so we report an error
      throwError $ DebugNodeConfigWrongGenesisHashCmdError configFilePath era actualHash expectedHash'
    (Just expectedHash', FixInPlace) -> do
      liftIO $
        Text.putStrLn $
          "Fixing " <> eraToStringKey cEra <> "'s genesis hash in " <> Text.pack configFilePath
      liftIO $ Text.putStrLn $ "Replacing " <> expectedHash' <> " with " <> actualHash
      pure $
        Aeson.insert (Aeson.fromText $ eraToGenesisHashKey era) (Aeson.String actualHash) configFileObj
 where
  configFilePath = unFile configFile

-- | Build an 'Analysis' value from the given node configuration file, for the given era.
-- Reads the genesis file path and hash from the node configuration file, if they exist.
mkAnalysis
  :: AnyCardanoEra
  -- ^ The era to analyze
  -> NodeConfigFile 'In
  -- ^ The node configuration file. It's not read by this function, but used for producing error messages.
  -> Aeson.Object
  -- ^ The node configuration file, as a JSON object
  -> ExceptT DebugCmdError IO (Maybe Analysis)
mkAnalysis cEra@(AnyCardanoEra era) configFile configFileObj = do
  case Aeson.lookup (Aeson.fromText $ eraToGenesisFileKey cEra) configFileObj of
    Nothing -> do
      -- The path to the genesis file is not given in the node configuration file.
      -- This is fine, except if a genesis hash is provided, because in this case;
      -- we can't check the hash. Which is why we write something to stdout.
      when
        (Aeson.member (Aeson.fromText $ eraToGenesisHashKey cEra) configFileObj)
        ( do
            liftIO $
              Text.putStrLn $
                "Genesis file for " <> eraToStringKey era <> " is not declared in " <> Text.pack configFilePath
            liftIO $ Text.putStrLn "So the hash's correctness cannot be checked."
        )
      pure Nothing
    Just (Aeson.String filepathText) -> do
      -- We make the genesis filepath relative to the node configuration file, like the node does:
      -- https://github.com/IntersectMBO/cardano-node/blob/9671e7b6a1b91f5a530722937949b86deafaad43/cardano-node/src/Cardano/Node/Configuration/POM.hs#L668
      -- Note that, if the genesis filepath is absolute, the node configuration file's directory is ignored (by property of </>)
      let genesisFilePath = takeDirectory configFilePath </> Text.unpack filepathText
      actualHash <-
        case era of
          ByronEra -> do
            (_, GenesisHash byronHash) <-
              firstExceptT (DebugNodeConfigGenesisDataCmdError genesisFilePath) $ readGenesisData genesisFilePath
            return $ Text.pack $ show byronHash
          _ -> Crypto.hashToTextAsHex <$> Read.readShelleyOnwardsGenesisAndHash genesisFilePath
      case Aeson.lookup (Aeson.fromText $ eraToGenesisHashKey cEra) configFileObj of
        Nothing ->
          pure $ Just $ Analysis cEra genesisFilePath actualHash Nothing
        Just (Aeson.String expectedHashText) ->
          pure $ Just $ Analysis cEra genesisFilePath actualHash (Just expectedHashText)
        Just _ ->
          throwE $
            DebugNodeConfigReadCmdError
              configFilePath
              "Expected genesis hash key to be mapped to a String value, but got something else"
    Just _ ->
      throwE $
        DebugNodeConfigReadCmdError
          configFilePath
          "Expected genesis file key to be mapped to a String value, but got something else"
 where
  configFilePath = unFile configFile

-- | The JSON key of the genesis hash for the given era.
eraToGenesisHashKey :: AnyCardanoEra -> Text.Text
eraToGenesisHashKey (AnyCardanoEra era) = eraToStringKey era <> "GenesisHash"

-- | The JSON key of the genesis file for the given era.
eraToGenesisFileKey :: AnyCardanoEra -> Text.Text
eraToGenesisFileKey (AnyCardanoEra era) = eraToStringKey era <> "GenesisFile"

-- | Part of the JSON keys associated with the given era. For example,
-- @eraToStringKey ByronEra@ returns @"Byron"@.
eraToStringKey :: CardanoEra a -> Text.Text
eraToStringKey = docToText . pretty
