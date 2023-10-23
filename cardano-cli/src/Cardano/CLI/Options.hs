{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Monoid law, left identity" -}

module Cardano.CLI.Options
  ( opts
  , pref
  ) where

import           Cardano.Api (CardanoEra (..), ShelleyBasedEra (..))

import           Cardano.CLI.Byron.Parsers (backwardsCompatibilityCommands, parseByronCommands)
import           Cardano.CLI.Environment (EnvCli)
import           Cardano.CLI.EraBased.Commands
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Legacy.Options (parseLegacyCmds)
import           Cardano.CLI.Render (customRenderHelp)
import           Cardano.CLI.Run (ClientCommand (..))
import           Cardano.CLI.Run.Ping (parsePingCmd)

import           Data.Foldable
import           Options.Applicative
import qualified Options.Applicative as Opt
import qualified Prettyprinter as PP

opts :: EnvCli -> ParserInfo ClientCommand
opts envCli =
  Opt.info (parseClientCommand envCli <**> Opt.helper) $ mconcat
    [ Opt.fullDesc
    , Opt.header $ mconcat
      [ "cardano-cli - General purpose command-line utility to interact with cardano-node."
      , " Provides specific commands to manage keys, addresses, build & submit transactions,"
      , " certificates, etc."
      ]
    ]

pref :: ParserPrefs
pref =
  Opt.prefs $ mconcat
    [ showHelpOnEmpty
    , helpEmbedBriefDesc PP.align
    , helpRenderHelp customRenderHelp
    ]

parseClientCommand :: EnvCli -> Parser ClientCommand
parseClientCommand envCli =
  asum
    -- There are name clashes between Shelley commands and the Byron backwards
    -- compat commands (e.g. "genesis"), and we need to prefer the Shelley ones
    -- so we list it first.
    [ parseAnyEra envCli
    , parseLegacy envCli
    -- , parseTopLevelLatest envCli -- TODO restore this when the governance command group is fully operational
    , parseTopLevelLegacy envCli
    , parseByron envCli
    , parsePing
    , backwardsCompatibilityCommands envCli
    , parseDisplayVersion (opts envCli)
    ]

parseByron :: EnvCli -> Parser ClientCommand
parseByron mNetworkId =
  fmap ByronCommand $
  subparser $ mconcat
    [ commandGroup "Byron specific commands"
    , metavar "Byron specific commands"
    , command' "byron" "Byron specific commands" $ parseByronCommands mNetworkId
    ]

{-
We don't care about the era here because this is simply a backwards compatible
command that is expected to work over all eras.

We need to be able to parse: Inputs, outputs, update proposals, stake registration, stake delegation and
pool registration is all that we need in previous eras.
-}
parseShelleyToBeforeMainnet :: EnvCli -> Parser ClientCommand
parseShelleyToBeforeMainnet _ = PreMainnetCommand <$> error "TODO"


parsePing :: Parser ClientCommand
parsePing = CliPingCommand <$> parsePingCmd

parseAnyEra :: EnvCli -> Parser ClientCommand
parseAnyEra envCli = AnyEraCommand <$> pAnyEraCommand envCli

parseLegacy :: EnvCli -> Parser ClientCommand
parseLegacy envCli =
  subParser "legacy"
    $ Opt.info (LegacyCmds <$> parseLegacyCmds envCli)
    $ Opt.progDesc "Legacy commands"

_parseTopLevelLatest :: EnvCli -> Parser ClientCommand
_parseTopLevelLatest envCli =
  AnyEraCommand . AnyEraCommandOf ShelleyBasedEraBabbage <$> pCmds BabbageEra envCli

-- | Parse Legacy commands at the top level of the CLI.
parseTopLevelLegacy :: EnvCli -> Parser ClientCommand
parseTopLevelLegacy envCli = LegacyCmds <$> parseLegacyCmds envCli

-- | Parse Legacy commands at the top level of the CLI.
-- Yes! A --version flag or version command. Either guess is right!
parseDisplayVersion :: ParserInfo a -> Parser ClientCommand
parseDisplayVersion allParserInfo =
  asum
    [ subparser $ mconcat
        [ commandGroup "Miscellaneous commands"
        , metavar "Miscellaneous commands"
        , command'
            "help"
            "Show all help"
            (pure (Help pref allParserInfo))
        , command'
            "version"
            "Show the cardano-cli version"
            (pure DisplayVersion)
        ]

    , flag' DisplayVersion $ mconcat
        [ long "version"
        , help "Show the cardano-cli version"
        , hidden
        ]
    ]
