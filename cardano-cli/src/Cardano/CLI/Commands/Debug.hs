module Cardano.CLI.Commands.Debug
  ( DebugCmds (..)
  )
where

import           Cardano.CLI.Commands.Debug.LogEpochState
import           Cardano.CLI.Commands.Debug.Transaction

data DebugCmds
  = DebugLogEpochStateCmd LogEpochStateCmdArgs
  | DebugTransactionCmds DebugTransactionCmds
