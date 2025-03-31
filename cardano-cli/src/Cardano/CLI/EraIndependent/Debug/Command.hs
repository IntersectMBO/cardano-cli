module Cardano.CLI.EraIndependent.Debug.Command
  ( DebugCmds (..)
  )
where

import Cardano.CLI.EraIndependent.Debug.CheckNodeConfiguration.Command
import Cardano.CLI.EraIndependent.Debug.LogEpochState.Command
import Cardano.CLI.EraIndependent.Debug.TransactionView.Command

data DebugCmds
  = DebugCheckNodeConfigurationCmd CheckNodeConfigCmdArgs
  | DebugLogEpochStateCmd LogEpochStateCmdArgs
  | DebugTransactionViewCmd TransactionViewCmdArgs
  | DebugTryIpcCmd
