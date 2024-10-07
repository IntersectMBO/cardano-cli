{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Monoid law, left identity" -}

module Cardano.CLI.Options
  ( opts
  , pref
  )
where

import           Cardano.CLI.Byron.Parsers (backwardsCompatibilityCommands, parseByronCommands)
import           Cardano.CLI.Environment (EnvCli)
import           Cardano.CLI.EraBased.Commands
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.EraBased.Options.Node
import           Cardano.CLI.Legacy.Options (parseLegacyCmds)
import           Cardano.CLI.Options.Debug
import           Cardano.CLI.Options.Hash
import           Cardano.CLI.Options.Ping (parsePingCmd)
import           Cardano.CLI.Render (customRenderHelp)
import           Cardano.CLI.Run (ClientCommand (..))

import           Data.Foldable
import           Options.Applicative
import qualified Options.Applicative as Opt
import qualified Prettyprinter as PP

opts :: EnvCli -> ParserInfo ClientCommand
opts envCli =
  Opt.info (parseClientCommand envCli <**> Opt.helper) $
    mconcat
      [ Opt.fullDesc
      , Opt.header $
          mconcat
            [ "cardano-cli - General purpose command-line utility to interact with cardano-node."
            , " Provides specific commands to manage keys, addresses, build & submit transactions,"
            , " certificates, etc."
            ]
      ]

pref :: ParserPrefs
pref =
  Opt.prefs $
    mconcat
      [ showHelpOnEmpty
      , helpEmbedBriefDesc PP.align
      , helpRenderHelp customRenderHelp
      ]

-- The node related commands are shelley era agnostic for the time being.
-- There is no need to guard them by the era argument.
nodeCmdsTopLevel :: Parser ClientCommand
nodeCmdsTopLevel = NodeCommands <$> pNodeCmds

parseClientCommand :: EnvCli -> Parser ClientCommand
parseClientCommand envCli =
  asum
    -- There are name clashes between Shelley commands and the Byron backwards
    -- compat commands (e.g. "genesis"), and we need to prefer the Shelley ones
    -- so we list it first.
    [ nodeCmdsTopLevel
    , parseLegacy envCli
    , parseByron envCli
    , parseAnyEra envCli
    , parseHash
    , parsePing
    , parseDebug envCli
    , backwardsCompatibilityCommands envCli
    , parseDisplayVersion (opts envCli)
    ]

parseByron :: EnvCli -> Parser ClientCommand
parseByron mNetworkId =
  fmap ByronCommand $
    subparser $
      mconcat
        [ commandGroup "Byron specific commands"
        , metavar "Byron specific commands"
        , command' "byron" "Byron specific commands" $ parseByronCommands mNetworkId
        ]

parseHash :: Parser ClientCommand
parseHash = HashCmds <$> pHashCmds

parsePing :: Parser ClientCommand
parsePing = CliPingCommand <$> parsePingCmd

parseDebug :: EnvCli -> Parser ClientCommand
parseDebug envCli = CliDebugCmds <$> parseDebugCmds envCli

parseAnyEra :: EnvCli -> Parser ClientCommand
parseAnyEra envCli = AnyEraCommand <$> pAnyEraCommand envCli

parseLegacy :: EnvCli -> Parser ClientCommand
parseLegacy envCli =
  subParser "legacy" $
    Opt.info (LegacyCmds <$> parseLegacyCmds envCli) $
      Opt.progDesc "Legacy commands"

-- | Parse Legacy commands at the top level of the CLI.
-- Yes! A --version flag or version command. Either guess is right!
parseDisplayVersion :: ParserInfo a -> Parser ClientCommand
parseDisplayVersion allParserInfo =
  asum
    [ subparser $
        mconcat
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
    , flag' DisplayVersion $
        mconcat
          [ long "version"
          , help "Show the cardano-cli version"
          , hidden
          ]
    ]
