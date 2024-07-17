module Cardano.CLI.Commands.Debug.Transaction
  ( DebugTransactionCmds (..)
  )
where

import           Cardano.CLI.Commands.Debug.Transaction.Echo

newtype DebugTransactionCmds
  = DebugTransactionEchoCmd TransactionEchoCmdArgs
