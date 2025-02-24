{-# LANGUAGE DataKinds #-}

module Cardano.CLI.EraIndependent.Debug.CheckNodeConfiguration.Command where

import Cardano.Api

-- | Argument for the 'debug check-node-configuration' command.
newtype CheckNodeConfigCmdArgs = CheckNodeConfigCmdArgs (NodeConfigFile 'In)
  deriving Show
