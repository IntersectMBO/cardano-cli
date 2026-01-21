{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Type.Error.DebugCmdError
  ( DebugCmdError (..)
  )
where

import Cardano.Api
import Cardano.Api.Byron qualified as Byron

import Cardano.CLI.Type.Error.TxCmdError

import Data.Text (Text)
import Data.Text.Lazy.Builder qualified as Text
import Formatting.Buildable (build)

data DebugCmdError
  = DebugCmdFailed
  | -- | @DebugNodeConfigGenesisDataCmdError filepath error@ represents an error when
    -- reading the node configuration at @filepath@
    DebugNodeConfigGenesisDataCmdError !FilePath !Byron.GenesisDataError
  | -- | @DebugNodeConfigWrongGenesisHashCmdError filepath eraGenesisPath actualHash expectedHash@ represents a user error
    -- that the hash of @eraGenesisPath@ in @filepath@ is @actualHash@, whereas it should be @expectedHash@
    DebugNodeConfigWrongGenesisHashCmdError
      !FilePath
      -- ^ The file path of the node configuration file
      !FilePath
      -- ^ The file path of the era configuration file whose hash is wrong
      !Text
      -- ^ The actual hash (the hash found by hashing the genesis file)
      !Text
      -- ^ The expected hash (the hash mentioned in the configuration file)
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
    DebugNodeConfigWrongGenesisHashCmdError nodeFp genesisFp actualHash expectedHash ->
      "Wrong genesis hash for "
        <> pretty genesisFp
        <> " in "
        <> pretty nodeFp
        <> ": when computing the hash, got: "
        <> pretty actualHash
        <> ", but the node configuration files states that this hash is expected: "
        <> pretty expectedHash
    DebugTxCmdError err -> renderTxCmdError err
