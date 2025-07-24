{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Dispatch for running all the CLI commands
module Cardano.CLI.Run
  ( ClientCommand (..)
  , runClientCommand
  )
where

import Cardano.Api

import Cardano.CLI.Byron.Run
import Cardano.CLI.Command
import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Compatible.Run
import Cardano.CLI.EraBased.Query.Run
import Cardano.CLI.EraBased.Run
import Cardano.CLI.EraIndependent.Address.Run
import Cardano.CLI.EraIndependent.Cip.Run
import Cardano.CLI.EraIndependent.Debug.Run
import Cardano.CLI.EraIndependent.Hash.Run (runHashCmds)
import Cardano.CLI.EraIndependent.Key.Run
import Cardano.CLI.EraIndependent.Node.Run
import Cardano.CLI.EraIndependent.Ping.Run
  ( runPingCmd
  )
import Cardano.CLI.Legacy.Run (runLegacyCmds)
import Cardano.CLI.Render (customRenderHelp)
import Cardano.Git.Rev (gitRev)

import RIO

import Data.List qualified as L
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Version (showVersion)
import Options.Applicative.Help.Core
import Options.Applicative.Types
  ( OptReader (..)
  , Option (..)
  , Parser (..)
  , ParserInfo (..)
  , ParserPrefs (..)
  )
import System.IO qualified as IO
import System.Info (arch, compilerName, compilerVersion, os)

import Paths_cardano_cli (version)

runClientCommand :: ClientCommand -> CIO e ()
runClientCommand = \case
  AnyEraCommand cmds ->
    runAnyEraCommand cmds
  AddressCommand cmds ->
    runAddressCmds cmds
  NodeCommands cmds ->
    runNodeCmds cmds
  ByronCommand cmds ->
    runByronClientCommand cmds
  CompatibleCommands cmd ->
    runAnyCompatibleCommand cmd
  HashCmds cmds ->
    runHashCmds cmds
  KeyCommands cmds ->
    runKeyCmds cmds
  LegacyCmds cmds ->
    runLegacyCmds cmds
  QueryCommands cmds ->
    runQueryCmds cmds
  CipFormatCmds cmds ->
    runCipFormat cmds
  CliPingCommand cmds ->
    runPingCmd cmds
  CliDebugCmds cmds ->
    runDebugCmds cmds
  Help pprefs allParserInfo ->
    runHelp pprefs allParserInfo
  DisplayVersion ->
    runDisplayVersion

runDisplayVersion :: CIO e ()
runDisplayVersion = do
  liftIO . Text.putStrLn $
    mconcat
      [ "cardano-cli "
      , renderVersion version
      , " - "
      , Text.pack os
      , "-"
      , Text.pack arch
      , " - "
      , Text.pack compilerName
      , "-"
      , renderVersion compilerVersion
      , "\ngit rev "
      , $(gitRev)
      ]
 where
  renderVersion = Text.pack . showVersion

helpAll :: ParserPrefs -> String -> [String] -> ParserInfo a -> IO ()
helpAll pprefs progn rnames parserInfo = do
  IO.putStrLn $ customRenderHelp 80 (usage_help parserInfo)
  IO.putStrLn ""
  go (infoParser parserInfo)
 where
  go :: Parser a -> IO ()
  go p = case p of
    NilP _ -> return ()
    OptP optP -> case optMain optP of
      CmdReader _ cs -> do
        forM_ cs $ \(c, subParserInfo) ->
          helpAll pprefs progn (c : rnames) subParserInfo
      _ -> return ()
    AltP pa pb -> go pa >> go pb
    MultP pf px -> go pf >> go px
    BindP pa _ -> go pa
  usage_help i =
    mconcat
      [ usageHelp (pure . parserUsage pprefs (infoParser i) . L.unwords $ progn : reverse rnames)
      , descriptionHelp (infoProgDesc i)
      ]

runHelp :: ParserPrefs -> ParserInfo a -> CIO e ()
runHelp pprefs allParserInfo = liftIO $ helpAll pprefs "cardano-cli" [] allParserInfo
