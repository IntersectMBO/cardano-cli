{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Run.Debug
  ( DebugCmdError (..)
  , runLogEpochStateCmd
  , runDebugCmds
  , renderDebugCmdError
  )
where

import           Cardano.Api

import           Cardano.CLI.Commands.Debug
import           Cardano.CLI.Run.Debug.LogEpochState
import           Cardano.CLI.Run.Debug.Transaction
import           Cardano.CLI.Types.Errors.DebugCmdError

runDebugCmds :: DebugCmds -> ExceptT DebugCmdError IO ()
runDebugCmds = \case
  DebugLogEpochStateCmd cmd -> liftIO $ runLogEpochStateCmd cmd
  DebugTransactionCmds cmds -> runDebugTransactionCmds cmds

renderDebugCmdError :: DebugCmdError -> Doc ann
renderDebugCmdError = \case
  DebugCmdFailed ->
    "Debug command failed"
  DebugCmdWriteFileError fileErr ->
    prettyError fileErr
  DebugCmdTextEnvCddlError cddlErr ->
    mconcat
      [ "Failed to decode the ledger's CDDL serialisation format. "
      , "TextEnvelopeCddl error: " <> prettyError cddlErr
      ]
