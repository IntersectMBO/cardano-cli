{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Type.Error.DebugCmdError
  ( DebugCmdError (..)
  )
where

import Cardano.Api
import Cardano.Api.Byron qualified as Byron
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Type.Error.TxCmdError
import Cardano.Configuration qualified as Cfg

import Data.Aeson.Key qualified as Aeson
import Data.Aeson.Types qualified as Aeson
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
    DebugNodeConfigReadCmdError fp err -> case err of
      -- cardano-config says which genesis file is at fault, and which key names it
      NodeConfigParsingError
        Cfg.ConfigurationParsingError
          { Cfg.errFile = L.SJust genesisFp
          , Cfg.errPath = [Aeson.Key key]
          , Cfg.errMessage = message
          }
          | genesisFp /= fp ->
              "Error reading the genesis file "
                <> pretty genesisFp
                <> " ("
                <> pretty (Aeson.toText key)
                <> " in "
                <> pretty fp
                <> "): "
                <> pretty message
      _ ->
        "Error reading node configuration at: "
          <> pretty fp
          <> ": "
          <> prettyError err
    DebugTxCmdError err -> renderTxCmdError err
