{-# LANGUAGE GADTs #-}

module Cardano.CLI.Commands
  ( ClientCommand (..)
  )
where

import           Cardano.CLI.Byron.Commands (ByronCommand)
import           Cardano.CLI.Commands.Debug
import           Cardano.CLI.Commands.Hash (HashCmds)
import           Cardano.CLI.Commands.Node
import           Cardano.CLI.Commands.Ping (PingCmd (..))
import           Cardano.CLI.EraBased.Commands
import           Cardano.CLI.Legacy.Commands

import           Options.Applicative.Types (ParserInfo (..), ParserPrefs (..))

-- | Sub-commands of 'cardano-cli'.
data ClientCommand
  = AnyEraCommand AnyEraCommand
  | -- | Byron Related Commands
    ByronCommand ByronCommand
  | -- | Era-agnostic hashing commands
    HashCmds HashCmds
  | -- | Era agnostic node commands
    NodeCommands NodeCmds
  | -- | Legacy shelley-based Commands
    LegacyCmds LegacyCmds
  | CliPingCommand PingCmd
  | CliDebugCmds DebugCmds
  | forall a. Help ParserPrefs (ParserInfo a)
  | DisplayVersion
