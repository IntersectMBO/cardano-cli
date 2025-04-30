{-# LANGUAGE GADTs #-}

module Cardano.CLI.Command
  ( ClientCommand (..)
  )
where

import Cardano.CLI.Byron.Command (ByronCommand)
import Cardano.CLI.Compatible.Command
import Cardano.CLI.EraBased.Command
import Cardano.CLI.EraBased.Query.Command
import Cardano.CLI.EraIndependent.Address.Command
import Cardano.CLI.EraIndependent.Cip.Command (CipFormatCmds)
import Cardano.CLI.EraIndependent.Debug.Command
import Cardano.CLI.EraIndependent.Hash.Command (HashCmds)
import Cardano.CLI.EraIndependent.Key.Command
import Cardano.CLI.EraIndependent.Node.Command
import Cardano.CLI.EraIndependent.Ping.Command (PingCmd)
import Cardano.CLI.Legacy.Command

import Options.Applicative.Types (ParserInfo (..), ParserPrefs (..))

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
  | -- | Miscellaneous commands
    CipFormatCmds CipFormatCmds
  | CliPingCommand PingCmd
  | CliDebugCmds DebugCmds
  | forall a. Help ParserPrefs (ParserInfo a)
  | DisplayVersion
