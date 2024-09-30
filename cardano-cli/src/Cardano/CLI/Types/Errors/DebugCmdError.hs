{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.DebugCmdError
  ( DebugCmdError (..)
  )
where

import           Cardano.Api

import           Cardano.Chain.Genesis
import           Cardano.CLI.Types.Errors.TxCmdError

import           Data.Text (Text)
import qualified Data.Text.Lazy.Builder as Text
import           Formatting.Buildable (build)

data DebugCmdError
  = DebugCmdFailed
  | -- | @DebugNodeConfigGenesisDataCmdError filepath error@ represents an error when
    -- reading the node configuration at @filepath@
    DebugNodeConfigGenesisDataCmdError !FilePath !GenesisDataError
  | -- | @DebugNodeConfigWrongGenesisHashCmdError filepath era actualHash expectedHash@ represents a user error
    -- that the genesis hash for @era@ in @filepath@ is @actualHash@, whereas it should be @expectedHash@
    DebugNodeConfigWrongGenesisHashCmdError
      !FilePath
      -- ^ The file path of the node configuration file
      AnyCardanoEra
      -- ^ The era whose data is incorrect
      !Text
      -- ^ The actual hash (the hash found by hashing the genesis file)
      !Text
      -- ^ The expected hash (the hash mentioned in the configuration file)
  | DebugTxCmdError !TxCmdError

instance Error DebugCmdError where
  prettyError = \case
    DebugCmdFailed -> "Debug command failed"
    DebugNodeConfigGenesisDataCmdError fp err ->
      "Error reading node configuration at: "
        <> pretty fp
        <> ": "
        <> pretty (Text.toLazyText $ build err)
    DebugNodeConfigWrongGenesisHashCmdError fp era actualHash expectedHash ->
      "Wrong genesis hash for "
        <> pretty era
        <> " in "
        <> pretty fp
        <> ": when computing the hash of the genesis file, got: "
        <> pretty actualHash
        <> ", but the node configuration files states that this hash is expected: "
        <> pretty expectedHash
    DebugTxCmdError err -> renderTxCmdError err
