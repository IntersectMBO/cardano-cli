{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Monoid law, left identity" -}

module Cardano.CLI.Option
  ( opts
  , pref
  )
where

import Cardano.CLI.Byron.Parser (backwardsCompatibilityCommands, parseByronCommands)
import Cardano.CLI.Compatible.Option
import Cardano.CLI.Environment (EnvCli)
import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraBased.Option
import Cardano.CLI.EraBased.Query.Option (pQueryCmdsTopLevel)
import Cardano.CLI.EraIndependent.Address.Option
import Cardano.CLI.EraIndependent.Cip.Option
import Cardano.CLI.EraIndependent.Debug.Option
import Cardano.CLI.EraIndependent.Hash.Option
import Cardano.CLI.EraIndependent.Key.Option
import Cardano.CLI.EraIndependent.Node.Option
import Cardano.CLI.EraIndependent.Ping.Option (parsePingCmd)
import Cardano.CLI.Legacy.Option (parseLegacyCmds)
import Cardano.CLI.Parser
import Cardano.CLI.Render (customRenderHelp)
import Cardano.CLI.Run (ClientCommand (..))

import Data.Foldable
import Options.Applicative
import Options.Applicative qualified as Opt
import Prettyprinter qualified as PP

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

addressCmdsTopLevel :: EnvCli -> Parser ClientCommand
addressCmdsTopLevel envCli = AddressCommand <$> pAddressCmds envCli

-- The node related commands are shelley era agnostic for the time being.
-- There is no need to guard them by the era argument.
nodeCmdsTopLevel :: Parser ClientCommand
nodeCmdsTopLevel = NodeCommands <$> pNodeCmds

-- Queries actually depend on the node to client version which may coincide
-- with a hardfork but not necessarily. We will expose commands at the top level
-- regardless if they are compatible with the era or not. The help text should be
-- updated to make this clear. Gating commands behind eras
queryCmdsTopLevel :: EnvCli -> Parser ClientCommand
queryCmdsTopLevel envCli = QueryCommands <$> pQueryCmdsTopLevel envCli

keyCmdsTopLevel :: Parser ClientCommand
keyCmdsTopLevel = KeyCommands <$> pKeyCmds

parseClientCommand :: EnvCli -> Parser ClientCommand
parseClientCommand envCli =
  asum
    -- There are name clashes between Shelley commands and the Byron backwards
    -- compat commands (e.g. "genesis"), and we need to prefer the Shelley ones
    -- so we list it first.
    [ addressCmdsTopLevel envCli
    , keyCmdsTopLevel
    , nodeCmdsTopLevel
    , parseHash
    , queryCmdsTopLevel envCli
    , parseLegacy envCli
    , parseByron envCli
    , parseAnyEra envCli
    , parseDebug envCli
    , backwardsCompatibilityCommands envCli
    , parseDisplayVersion (opts envCli)
    , parseCipCmd
    , parseCompatibilityCommands envCli
    ]

parseByron :: EnvCli -> Parser ClientCommand
parseByron mNetworkId =
  fmap ByronCommand $
    subparser $
      mconcat
        [ metavar "Byron specific commands"
        , command' "byron" "Byron specific commands" $ parseByronCommands mNetworkId
        ]

parseHash :: Parser ClientCommand
parseHash = HashCmds <$> pHashCmds

parseCompatibilityCommands :: EnvCli -> Parser ClientCommand
parseCompatibilityCommands envCli =
  Opt.hsubparser $
    commandWithMetavar "compatible" $
      Opt.info (CompatibleCommands <$> pAnyCompatibleCommand envCli) $
        Opt.progDesc "Limited backward compatible commands for testing only."

parseDebug :: EnvCli -> Parser ClientCommand
parseDebug envCli = CliDebugCmds <$> parseDebugCmds envCli

parseAnyEra :: EnvCli -> Parser ClientCommand
parseAnyEra envCli = AnyEraCommand <$> pAnyEraCommand envCli

parseLegacy :: EnvCli -> Parser ClientCommand
parseLegacy envCli =
  Opt.hsubparser $
    commandWithMetavar "legacy" $
      Opt.info (LegacyCmds <$> parseLegacyCmds envCli) $
        Opt.progDesc "Legacy commands"

-- | Parse Legacy commands at the top level of the CLI.
-- Yes! A --version flag or version command. Either guess is right!
parseDisplayVersion :: ParserInfo a -> Parser ClientCommand
parseDisplayVersion allParserInfo =
  asum
    [ subparser $
        mconcat
          [ metavar "Miscellaneous commands"
          , command'
              "help"
              "Show all help"
              (pure (Help pref allParserInfo))
          , command'
              "version"
              "Show the cardano-cli version"
              (pure DisplayVersion)
          , parsePingCmd
          ]
    , flag' DisplayVersion $
        mconcat
          [ long "version"
          , help "Show the cardano-cli version"
          , hidden
          ]
    ]
