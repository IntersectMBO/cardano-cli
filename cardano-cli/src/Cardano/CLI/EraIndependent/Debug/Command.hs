module Cardano.CLI.EraIndependent.Debug.Command
  ( DebugCmds (..)
  )
where

import Cardano.CLI.EraIndependent.Debug.CheckNodeConfiguration.Command
import Cardano.CLI.EraIndependent.Debug.CheckPoolRegistration.Command
import Cardano.CLI.EraIndependent.Debug.LogEpochState.Command
import Cardano.CLI.EraIndependent.Debug.TransactionView.Command

data DebugCmds
  = DebugCheckNodeConfigurationCmd CheckNodeConfigCmdArgs
  | DebugCheckPoolRegistrationCmd CheckPoolRegistrationCmdArgs
  | DebugLogEpochStateCmd LogEpochStateCmdArgs
  | DebugTransactionViewCmd TransactionViewCmdArgs
