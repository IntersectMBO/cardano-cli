{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Type.Error.DebugCmdError
  ( DebugCmdError (..)
  )
where

import Cardano.Api
import Cardano.Api.Byron qualified as Byron

import Cardano.CLI.Type.Error.TxCmdError

import Data.Text.Lazy.Builder qualified as Text
import Formatting.Buildable (build)

data DebugCmdError
  = DebugCmdFailed
  | -- | @DebugNodeConfigGenesisDataCmdError filepath error@ represents an error when
    -- reading the node configuration at @filepath@
    DebugNodeConfigGenesisDataCmdError !FilePath !Byron.GenesisDataError
  | -- | @DebugNodeConfigReadCmdError filepath error@ represents an error when
    -- reading the node configuration at @filepath@, or the genesis files it names
    DebugNodeConfigReadCmdError !FilePath !NodeConfigError
  | DebugTxCmdError !TxCmdError
  deriving Show

instance Error DebugCmdError where
  prettyError = \case
    DebugCmdFailed -> "Debug command failed"
    DebugNodeConfigGenesisDataCmdError fp err ->
      "Error reading node configuration at: "
        <> pretty fp
        <> ": "
        <> pretty (Text.toLazyText $ build err)
    DebugNodeConfigReadCmdError fp err ->
      "Error reading node configuration at: "
        <> pretty fp
        <> ": "
        <> prettyError err
    DebugTxCmdError err -> renderTxCmdError err
