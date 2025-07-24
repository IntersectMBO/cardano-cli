{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.EraIndependent.Debug.Run
  ( DebugCmdError (..)
  , runLogEpochStateCmd
  , runDebugCmds
  )
where

import Cardano.Api

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraIndependent.Debug.CheckNodeConfiguration.Run (runCheckNodeConfig)
import Cardano.CLI.EraIndependent.Debug.Command
import Cardano.CLI.EraIndependent.Debug.LogEpochState.Run
import Cardano.CLI.EraIndependent.Debug.TransactionView.Run (runTransactionViewCmd)
import Cardano.CLI.Type.Error.DebugCmdError

runDebugCmds :: DebugCmds -> CIO e ()
runDebugCmds = \case
  DebugCheckNodeConfigurationCmd cmd -> runCheckNodeConfig cmd
  DebugLogEpochStateCmd cmd -> liftIO $ runLogEpochStateCmd cmd
  DebugTransactionViewCmd cmd -> runTransactionViewCmd cmd
