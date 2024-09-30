{-# LANGUAGE DataKinds #-}

module Cardano.CLI.Commands.Debug.CheckNodeConfiguration where

import           Cardano.Api

import           Cardano.CLI.Types.Common (FixNodeConfigFile)

-- | Argument for the 'debug check-node-configuration' command.
data CheckNodeConfigCmdArgs = CheckNodeConfigCmdArgs
  { configFileToCheck :: !(NodeConfigFile 'In)
  , fixTheFile :: FixNodeConfigFile
  }
  deriving Show
