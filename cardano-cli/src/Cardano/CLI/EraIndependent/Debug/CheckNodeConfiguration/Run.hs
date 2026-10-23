{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.EraIndependent.Debug.CheckNodeConfiguration.Run (runCheckNodeConfig) where

import Cardano.Api

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraIndependent.Debug.CheckNodeConfiguration.Command

import Control.Monad (void)

runCheckNodeConfig :: CheckNodeConfigCmdArgs -> CIO e ()
runCheckNodeConfig (CheckNodeConfigCmdArgs configFile) = do
  -- The API loader reads the genesis files and validates their configured hashes.
  void $ fromExceptTCli $ readNodeConfig configFile
  liftIO $ putStrLn $ "Successfully checked node configuration file: " <> unFile configFile
