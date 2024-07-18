{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Dispatch for running all the CLI commands
module Cardano.CLI.Run
  ( ClientCommand (..)
  , ClientCommandErrors
  , renderClientCommandError
  , runClientCommand
  )
where

import           Cardano.Api

import           Cardano.CLI.Byron.Run (ByronClientCmdError, renderByronClientCmdError,
                   runByronClientCommand)
import           Cardano.CLI.Commands
import           Cardano.CLI.EraBased.Commands
import           Cardano.CLI.EraBased.Run
import           Cardano.CLI.Legacy.Commands
import           Cardano.CLI.Legacy.Run (runLegacyCmds)
import           Cardano.CLI.Render (customRenderHelp)
import           Cardano.CLI.Run.Debug
import           Cardano.CLI.Run.Hash (runHashCmds)
import           Cardano.CLI.Run.Ping (PingClientCmdError (..), renderPingClientCmdError,
                   runPingCmd)
import           Cardano.CLI.Types.Errors.CmdError
import           Cardano.CLI.Types.Errors.HashCmdError
import           Cardano.Git.Rev (gitRev)

import           Control.Monad (forM_)
import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Version (showVersion)
import           Options.Applicative.Help.Core
import           Options.Applicative.Types (OptReader (..), Option (..), Parser (..),
                   ParserInfo (..), ParserPrefs (..))
import           System.Info (arch, compilerName, compilerVersion, os)
import qualified System.IO as IO

import           Paths_cardano_cli (version)

data ClientCommandErrors
  = ByronClientError ByronClientCmdError
  | CmdError Text CmdError
  | HashCmdError HashCmdError
  | PingClientError PingClientCmdError
  | DebugCmdError DebugCmdError

runClientCommand :: ClientCommand -> ExceptT ClientCommandErrors IO ()
runClientCommand = \case
  AnyEraCommand cmds ->
    firstExceptT (CmdError (renderAnyEraCommand cmds)) $ runAnyEraCommand cmds
  ByronCommand cmds ->
    firstExceptT ByronClientError $ runByronClientCommand cmds
  HashCmds cmds ->
    firstExceptT HashCmdError $ runHashCmds cmds
  LegacyCmds cmds ->
    firstExceptT (CmdError (renderLegacyCommand cmds)) $ runLegacyCmds cmds
  CliPingCommand cmds ->
    firstExceptT PingClientError $ runPingCmd cmds
  CliDebugCmds cmds ->
    firstExceptT DebugCmdError $ runDebugCmds cmds
  Help pprefs allParserInfo ->
    runHelp pprefs allParserInfo
  DisplayVersion ->
    runDisplayVersion

renderClientCommandError :: ClientCommandErrors -> Doc ann
renderClientCommandError = \case
  CmdError cmdText err ->
    renderCmdError cmdText err
  ByronClientError err ->
    renderByronClientCmdError err
  HashCmdError err ->
    prettyError err
  PingClientError err ->
    renderPingClientCmdError err
  DebugCmdError err ->
    prettyError err

runDisplayVersion :: ExceptT ClientCommandErrors IO ()
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

runHelp :: ParserPrefs -> ParserInfo a -> ExceptT ClientCommandErrors IO ()
runHelp pprefs allParserInfo = liftIO $ helpAll pprefs "cardano-cli" [] allParserInfo
