{-# LANGUAGE GADTs #-}

module Cardano.CLI.Commands
  ( ClientCommand(..)
  ) where

import           Cardano.CLI.Byron.Commands (ByronCommand)
import           Cardano.CLI.EraBased.Commands
import           Cardano.CLI.Legacy.Commands
import           Cardano.CLI.Run.Ping (PingCmd (..))

import           Options.Applicative.Types (ParserInfo (..), ParserPrefs (..))

-- | Sub-commands of 'cardano-cli'.
data ClientCommand =
    AnyEraCommand AnyEraCommand

    -- | Byron Related Commands
  | ByronCommand ByronCommand

    -- | Legacy shelley-based Commands
  | LegacyCmds LegacyCmds

  | CliPingCommand PingCmd

  | forall a. Help ParserPrefs (ParserInfo a)
  | DisplayVersion
