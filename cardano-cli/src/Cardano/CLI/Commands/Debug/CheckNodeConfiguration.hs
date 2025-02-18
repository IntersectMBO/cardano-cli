{-# LANGUAGE DataKinds #-}

module Cardano.CLI.Commands.Debug.CheckNodeConfiguration where

import Cardano.Api

-- | Argument for the 'debug check-node-configuration' command.
newtype CheckNodeConfigCmdArgs = CheckNodeConfigCmdArgs (NodeConfigFile 'In)
  deriving Show
