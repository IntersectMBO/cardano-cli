{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraIndependent.Debug.Run
  ( DebugCmdError (..)
  , runLogEpochStateCmd
  , runDebugCmds
  )
where

import Cardano.Api

import Cardano.CLI.EraIndependent.Debug.CheckNodeConfiguration.Run (runCheckNodeConfig)
import Cardano.CLI.EraIndependent.Debug.Command
import Cardano.CLI.EraIndependent.Debug.LogEpochState.Run
import Cardano.CLI.EraIndependent.Debug.TransactionView.Run (runTransactionViewCmd)
import Cardano.CLI.EraIndependent.Debug.TryIpc.Run (runTryIpcCmd)
import Cardano.CLI.Type.Error.DebugCmdError

runDebugCmds :: DebugCmds -> ExceptT DebugCmdError IO ()
runDebugCmds = \case
  DebugCheckNodeConfigurationCmd cmd -> runCheckNodeConfig cmd
  DebugLogEpochStateCmd cmd -> liftIO $ runLogEpochStateCmd cmd
  DebugTransactionViewCmd cmd -> firstExceptT DebugTxCmdError $ runTransactionViewCmd cmd
  DebugTryIpcCmd -> liftIO runTryIpcCmd
