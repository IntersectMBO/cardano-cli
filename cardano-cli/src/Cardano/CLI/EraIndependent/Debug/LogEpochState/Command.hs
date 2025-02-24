{-# LANGUAGE DataKinds #-}

module Cardano.CLI.EraIndependent.Debug.LogEpochState.Command
  ( LogEpochStateCmdArgs (..)
  , Configuration
  )
where

import Cardano.Api

import Cardano.CLI.Orphan ()

-- | A phantom type to represent the configuration file.
data Configuration

-- | The arguments for the 'debug log-epoch-state' command.
--
-- This command will connect to a local node and log the epoch state.
data LogEpochStateCmdArgs = LogEpochStateCmdArgs
  { nodeSocketPath :: !SocketPath
  , configurationFile :: !(NodeConfigFile 'In)
  , outputFilePath :: !(File Configuration 'Out)
  }
  deriving Show
