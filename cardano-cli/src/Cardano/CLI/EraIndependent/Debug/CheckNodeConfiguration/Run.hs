{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraIndependent.Debug.CheckNodeConfiguration.Run (runCheckNodeConfig) where

import Cardano.Api

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraIndependent.Debug.CheckNodeConfiguration.Command
import Cardano.CLI.Type.Error.DebugCmdError

import System.IO qualified as IO

-- | Check a node configuration file by reading it the way 'readNodeConfig'
-- does: every genesis file it names must exist, decode, and match the hash the
-- configuration gives for it.
runCheckNodeConfig :: CheckNodeConfigCmdArgs -> CIO e ()
runCheckNodeConfig (CheckNodeConfigCmdArgs configFile) = do
  _ <-
    fromExceptTCli . modifyError (DebugNodeConfigReadCmdError configFilePath) $
      readNodeConfig configFile
  liftIO $
    IO.hPutStrLn IO.stderr $
      "Successfully checked node configuration file: " <> configFilePath
 where
  configFilePath = unFile configFile
