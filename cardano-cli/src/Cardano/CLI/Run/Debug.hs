{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Run.Debug
  ( DebugCmdError (..)
  , runLogEpochStateCmd
  , runDebugCmds
  )
where

import           Cardano.Api

import           Cardano.CLI.Commands.Debug
import           Cardano.CLI.Run.Debug.CheckNodeConfiguration (runCheckNodeConfig)
import           Cardano.CLI.Run.Debug.LogEpochState
import           Cardano.CLI.Run.Debug.TransactionView (runTransactionViewCmd)
import           Cardano.CLI.Types.Errors.DebugCmdError

runDebugCmds :: DebugCmds -> ExceptT DebugCmdError IO ()
runDebugCmds = \case
  DebugCheckNodeConfigurationCmd cmd -> runCheckNodeConfig cmd
  DebugLogEpochStateCmd cmd -> liftIO $ runLogEpochStateCmd cmd
  DebugTransactionViewCmd cmd -> firstExceptT DebugTxCmdError $ runTransactionViewCmd cmd
