{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- | Dispatch for running all the CLI commands
module Cardano.CLI.Run
  ( ClientCommand(..)
  , ClientCommandErrors
  , renderClientCommandError
  , runClientCommand
  ) where

import           Cardano.Api (Error (..))

import           Cardano.CLI.Byron.Commands (ByronCommand)
import           Cardano.CLI.Byron.Run (ByronClientCmdError, renderByronClientCmdError,
                   runByronClientCommand)
import           Cardano.CLI.EraBased.Commands
import           Cardano.CLI.EraBased.Run
import           Cardano.CLI.Legacy.Commands (LegacyCmds)
import           Cardano.CLI.Legacy.Run (LegacyClientCmdError, renderLegacyClientCmdError,
                   runLegacyCmds)
import           Cardano.CLI.Render (customRenderHelp)
import           Cardano.CLI.Run.Ping (PingClientCmdError (..), PingCmd (..),
                   renderPingClientCmdError, runPingCmd)
import           Cardano.Git.Rev (gitRev)

import           Control.Monad (forM_)
import           Control.Monad.IO.Unlift (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)
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

data ClientCommandErrors
  = AnyEraCmdError AnyEraCmdError
  | ByronClientError ByronClientCmdError
  | LegacyClientError LegacyCmds LegacyClientCmdError
  | PingClientError PingClientCmdError

runClientCommand :: ClientCommand -> ExceptT ClientCommandErrors IO ()
runClientCommand = \case
  AnyEraCommand cmd ->
    firstExceptT AnyEraCmdError $ runAnyEraCommand cmd
  ByronCommand c ->
    firstExceptT ByronClientError $ runByronClientCommand c
  LegacyCmds c ->
    firstExceptT (LegacyClientError c) $ runLegacyCmds c
  CliPingCommand c ->
    firstExceptT PingClientError $ runPingCmd c
  Help pprefs allParserInfo ->
    runHelp pprefs allParserInfo
  DisplayVersion ->
    runDisplayVersion

renderClientCommandError :: ClientCommandErrors -> Text
renderClientCommandError = \case
  AnyEraCmdError err ->
    Text.pack $ displayError err
  ByronClientError err ->
    renderByronClientCmdError err
  LegacyClientError cmd err ->
    renderLegacyClientCmdError cmd err
  PingClientError err ->
    renderPingClientCmdError err

runDisplayVersion :: ExceptT ClientCommandErrors IO ()
runDisplayVersion = do
  liftIO . Text.putStrLn $ mconcat
    [ "cardano-cli ", renderVersion version
    , " - ", Text.pack os, "-", Text.pack arch
    , " - ", Text.pack compilerName, "-", renderVersion compilerVersion
    , "\ngit rev ", gitRev
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
              helpAll pprefs progn (c:rnames) subParserInfo
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
