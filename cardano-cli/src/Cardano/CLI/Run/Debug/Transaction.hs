{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Run.Debug.Transaction
  ( runDebugTransactionCmds
  )
where

import           Cardano.Api

import           Cardano.CLI.Commands.Debug.Transaction
import           Cardano.CLI.Run.Debug.Transaction.Echo
import           Cardano.CLI.Types.Errors.DebugCmdError

runDebugTransactionCmds
  :: DebugTransactionCmds
  -> ExceptT DebugCmdError IO ()
runDebugTransactionCmds = \case
  DebugTransactionEchoCmd cmd -> runDebugTransactionEchoCmd cmd
