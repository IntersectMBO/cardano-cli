module Cardano.CLI.Commands.Debug
  ( DebugCmds (..)
  )
where

import Cardano.CLI.Commands.Debug.CheckNodeConfiguration
import Cardano.CLI.Commands.Debug.LogEpochState
import Cardano.CLI.Commands.Debug.TransactionView

data DebugCmds
  = DebugCheckNodeConfigurationCmd CheckNodeConfigCmdArgs
  | DebugLogEpochStateCmd LogEpochStateCmdArgs
  | DebugTransactionViewCmd TransactionViewCmdArgs
