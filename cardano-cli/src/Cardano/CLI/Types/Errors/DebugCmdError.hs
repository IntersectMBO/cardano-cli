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
  | -- | @DebugNodeConfigReadCmdError filepath error@ represents an error when
    -- reading the node configuration at @filepath@
    DebugNodeConfigReadCmdError !FilePath !String
  | -- | @DebugNodeConfigWrongGenesisHashCmdError filepath era actualHash expectedHash@ represents a user error
    -- that the genesis hash for @era@ in @filepath@ is @actualHash@, whereas it should be @expectedHash@
    DebugNodeConfigWrongGenesisHashCmdError !FilePath AnyCardanoEra !Text !Text
  | DebugTxCmdError !TxCmdError

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
        <> pretty err
    DebugNodeConfigWrongGenesisHashCmdError fp era actualHash expectedHash ->
      "Wrong genesis hash for "
        <> pretty era
        <> " in "
        <> pretty fp
        <> ": "
        <> pretty actualHash
        <> ", expected: "
        <> pretty expectedHash
    DebugTxCmdError err -> renderTxCmdError err
