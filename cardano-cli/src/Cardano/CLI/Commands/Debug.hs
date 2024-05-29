module Cardano.CLI.Commands.Debug
  ( DebugCmds (..)
  ) where

import           Cardano.CLI.Commands.Debug.LogEpochState

newtype DebugCmds =
  DebugLogEpochStateCmd LogEpochStateCmdArgs
