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

import Cardano.Api

import Cardano.CLI.Byron.Run
import Cardano.CLI.Command
import Cardano.CLI.Compatible.Command
import Cardano.CLI.Compatible.Run
import Cardano.CLI.EraBased.Command
import Cardano.CLI.EraBased.Query.Command (renderQueryCmds)
import Cardano.CLI.EraBased.Query.Run
import Cardano.CLI.EraBased.Run
import Cardano.CLI.EraIndependent.Address.Command
import Cardano.CLI.EraIndependent.Address.Run
import Cardano.CLI.EraIndependent.Cip.Command
import Cardano.CLI.EraIndependent.Cip.Run
import Cardano.CLI.EraIndependent.Debug.Run
import Cardano.CLI.EraIndependent.Hash.Command
import Cardano.CLI.EraIndependent.Hash.Run (runHashCmds)
import Cardano.CLI.EraIndependent.Key.Command
import Cardano.CLI.EraIndependent.Key.Run
import Cardano.CLI.EraIndependent.Node.Command
import Cardano.CLI.EraIndependent.Node.Run
import Cardano.CLI.EraIndependent.Ping.Run
  ( PingClientCmdError (..)
  , renderPingClientCmdError
  , runPingCmd
  )
import Cardano.CLI.Legacy.Command
import Cardano.CLI.Legacy.Run (runLegacyCmds)
import Cardano.CLI.Render (customRenderHelp, renderAnyCmdError)
import Cardano.CLI.Type.Error.AddressCmdError
import Cardano.CLI.Type.Error.CmdError
import Cardano.CLI.Type.Error.HashCmdError
import Cardano.CLI.Type.Error.KeyCmdError
import Cardano.CLI.Type.Error.NodeCmdError
import Cardano.CLI.Type.Error.QueryCmdError
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

data ClientCommandErrors
  = ByronClientError ByronClientCmdError
  | AddressCmdError AddressCmdError
  | CmdError Text CmdError
  | BackwardCompatibleError
      Text
      -- ^ Command that was run
      SomeException
      -- ^ An exception that was thrown
  | HashCmdError HashCmdError
  | KeyCmdError KeyCmdError
  | NodeCmdError NodeCmdError
  | QueryCmdError QueryCmdError
  | PingClientError PingClientCmdError
  | DebugCmdError DebugCmdError

runClientCommand :: ClientCommand -> ExceptT ClientCommandErrors IO ()
runClientCommand = \case
  AnyEraCommand cmds ->
    firstExceptT (CmdError (renderAnyEraCommand cmds)) $ runAnyEraCommand cmds
  AddressCommand cmds ->
    newExceptT $
      runRIO () $
        catch
          (Right <$> runAddressCmds cmds)
          (pure . Left . BackwardCompatibleError (renderAddressCmds cmds))
  NodeCommands cmds ->
    newExceptT $
      runRIO () $
        catch
          (Right <$> runNodeCmds cmds)
          (pure . Left . BackwardCompatibleError (renderNodeCmds cmds))
  ByronCommand cmds ->
    newExceptT $
      runRIO () $
        catch
          (Right <$> runByronClientCommand cmds)
          -- TODO: Render byron commands properly
          (pure . Left . BackwardCompatibleError (Text.pack $ show cmds))
  CompatibleCommands cmd ->
    -- Catch an exception and wrap it in ExceptT error in order to reuse existing error printing
    -- facilities
    -- TODO This needs to be changed in the future to let the top level exception handler handle the
    -- exceptions printing.
    newExceptT $
      runRIO () $
        catch
          (Right <$> runAnyCompatibleCommand cmd)
          (pure . Left . BackwardCompatibleError (renderAnyCompatibleCommand cmd))
  HashCmds cmds ->
    newExceptT $
      runRIO () $
        catch
          (Right <$> runHashCmds cmds)
          (pure . Left . BackwardCompatibleError (renderHashCmds cmds))
  KeyCommands cmds ->
    newExceptT $
      runRIO () $
        catch
          (Right <$> runKeyCmds cmds)
          (pure . Left . BackwardCompatibleError (renderKeyCmds cmds))
  LegacyCmds cmds ->
    newExceptT $
      runRIO () $
        catch
          (Right <$> runLegacyCmds cmds)
          (pure . Left . BackwardCompatibleError (renderLegacyCommand cmds))
  QueryCommands cmds ->
    newExceptT $
      runRIO () $
        catch
          (Right <$> runQueryCmds cmds)
          (pure . Left . BackwardCompatibleError (renderQueryCmds cmds))
  CipFormatCmds cmds ->
    newExceptT $
      runRIO () $
        catch
          (Right <$> runCipFormat cmds)
          (pure . Left . BackwardCompatibleError (renderCipFormatCmds cmds))
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
  AddressCmdError err ->
    renderAddressCmdError err
  BackwardCompatibleError cmdText err ->
    renderAnyCmdError cmdText prettyException err
  HashCmdError err ->
    prettyError err
  NodeCmdError err ->
    renderNodeCmdError err
  KeyCmdError err ->
    renderKeyCmdError err
  QueryCmdError err ->
    renderQueryCmdError err
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
