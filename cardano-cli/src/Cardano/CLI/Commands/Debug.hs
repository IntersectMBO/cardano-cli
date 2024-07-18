module Cardano.CLI.Commands.Debug
  ( DebugCmds (..)
  )
where

import           Cardano.CLI.Commands.Debug.LogEpochState
import           Cardano.CLI.Commands.Debug.TransactionView

data DebugCmds
  = DebugLogEpochStateCmd LogEpochStateCmdArgs
  | DebugTransactionViewCmd TransactionViewCmdArgs
