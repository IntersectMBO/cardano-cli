{-# LANGUAGE GADTs #-}

module Cardano.CLI.Commands
  ( ClientCommand (..)
  )
where

import           Cardano.CLI.Byron.Commands (ByronCommand)
import           Cardano.CLI.Commands.Address
import           Cardano.CLI.Commands.Debug
import           Cardano.CLI.Commands.Hash (HashCmds)
import           Cardano.CLI.Commands.Key
import           Cardano.CLI.Commands.Node
import           Cardano.CLI.Commands.Ping (PingCmd (..))
import           Cardano.CLI.Compatible.Commands
import           Cardano.CLI.EraBased.Commands.Query
import           Cardano.CLI.EraBased.Commands.TopLevelCommands
import           Cardano.CLI.Legacy.Commands

import           Options.Applicative.Types (ParserInfo (..), ParserPrefs (..))

-- | Sub-commands of 'cardano-cli'.
data ClientCommand
  = AnyEraCommand AnyEraCommand
  | AddressCommand AddressCmds
  | -- | Byron Related Commands
    ByronCommand ByronCommand
  | -- | Backward compatible commands for testing only
    CompatibleCommands AnyCompatibleCommand
  | -- | Era-agnostic hashing commands
    HashCmds HashCmds
  | -- | Era agnostic key commands
    KeyCommands KeyCmds
  | -- | Era agnostic node commands
    NodeCommands NodeCmds
  | -- | Query commands
    forall era. QueryCommands (QueryCmds era)
  | -- | Legacy shelley-based Commands
    LegacyCmds LegacyCmds
  | CliPingCommand PingCmd
  | CliDebugCmds DebugCmds
  | forall a. Help ParserPrefs (ParserInfo a)
  | DisplayVersion
