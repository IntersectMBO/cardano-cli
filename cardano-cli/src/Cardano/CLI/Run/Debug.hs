{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Run.Debug
  ( DebugCmdError(..)
  , runLogEpochStateCmd
  , runDebugCmds
  , renderDebugCmdError
  ) where

import           Cardano.CLI.Commands.Debug
import           Cardano.CLI.Run.Debug.LogEpochState

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Prettyprinter

data DebugCmdError = DebugCmdFailed

runDebugCmds :: DebugCmds -> ExceptT DebugCmdError IO ()
runDebugCmds = \case
  DebugLogEpochStateCmd cmd -> liftIO $ runLogEpochStateCmd cmd

renderDebugCmdError :: DebugCmdError -> Doc ann
renderDebugCmdError DebugCmdFailed = "Debug command failed"
