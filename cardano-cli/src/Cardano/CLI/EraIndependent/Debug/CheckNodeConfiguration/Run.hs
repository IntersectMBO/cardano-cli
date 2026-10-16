{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraIndependent.Debug.CheckNodeConfiguration.Run (runCheckNodeConfig) where

import Cardano.Api

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraIndependent.Debug.CheckNodeConfiguration.Command
import Cardano.CLI.Type.Error.DebugCmdError

-- | Check a node configuration file by reading it the way 'readNodeConfig'
-- does: every genesis file it names must exist, decode, and match the hash the
-- configuration gives for it.
runCheckNodeConfig :: CheckNodeConfigCmdArgs -> CIO e ()
runCheckNodeConfig (CheckNodeConfigCmdArgs configFile) = do
  result <- liftIO . runExceptT $ readNodeConfig configFile
  case result of
    Left err -> throwCliError $ DebugNodeConfigReadCmdError configFilePath err
    Right _ -> liftIO $ putStrLn $ "Successfully checked node configuration file: " <> configFilePath
 where
  configFilePath = unFile configFile
