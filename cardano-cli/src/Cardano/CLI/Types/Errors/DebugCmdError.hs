{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.DebugCmdError
  ( DebugCmdError (..)
  )
where

import           Cardano.Api

import           Cardano.CLI.Types.Errors.TxCmdError

data DebugCmdError
  = DebugCmdFailed
  | DebugTxCmdError !TxCmdError

instance Error DebugCmdError where
  prettyError = \case
    DebugCmdFailed -> "Debug command failed"
    DebugTxCmdError err -> renderTxCmdError err
