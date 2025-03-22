{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

module Cardano.CLI.EraIndependent.Debug.Option
  ( parseDebugCmds
  )
where

import Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import Cardano.CLI.Environment
import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraIndependent.Debug.CheckNodeConfiguration.Command
import Cardano.CLI.EraIndependent.Debug.Command
import Cardano.CLI.EraIndependent.Debug.LogEpochState.Command
import Cardano.CLI.EraIndependent.Debug.TransactionView.Command
import Cardano.CLI.Parser

import Data.Foldable
import Options.Applicative hiding (help, str)
import Options.Applicative qualified as Opt

parseDebugCmds :: EnvCli -> Parser DebugCmds
parseDebugCmds envCli =
  Opt.hsubparser $
    mconcat
      [ Opt.metavar "debug commands"
      , Opt.commandGroup "debug commands"
      , Opt.command "debug" $
          Opt.info (pDebugCmds envCli) $
            Opt.progDesc "Debug commands"
      ]

pDebugCmds :: EnvCli -> Parser DebugCmds
pDebugCmds envCli =
  asum
    [ Opt.hsubparser $
        commandWithMetavar "log-epoch-state" $
          Opt.info pLogEpochStateCmdArgs $
            Opt.progDesc $
              mconcat
                [ "Log epoch state of a running node."
                , " This command will connect to a local node and log the epoch state to a file."
                , " The log file format is line delimited JSON."
                , " The command will not terminate."
                ]
    , Opt.hsubparser $
        commandWithMetavar "check-node-configuration" $
          Opt.info pCheckNodeConfigurationCmdArgs $
            Opt.progDesc
              "Check hashes and paths of genesis files in the given node configuration file."
    , Opt.hsubparser $
        commandWithMetavar "transaction" $
          Opt.info
            ( asum
                [ Opt.hsubparser $
                    commandWithMetavar "view" $
                      Opt.info pTransactionView $
                        Opt.progDesc "Print a transaction."
                ]
            )
            (Opt.progDesc "Transaction commands")
    ]
 where
  pLogEpochStateCmdArgs :: Parser DebugCmds
  pLogEpochStateCmdArgs =
    fmap DebugLogEpochStateCmd $
      LogEpochStateCmdArgs
        <$> pSocketPath envCli
        <*> pNodeConfigurationFileIn
        <*> pFileOutDirection
          "out-file"
          "Output filepath of the log file.  The log file format is line delimited JSON."
  pCheckNodeConfigurationCmdArgs :: Parser DebugCmds
  pCheckNodeConfigurationCmdArgs =
    fmap DebugCheckNodeConfigurationCmd $
      CheckNodeConfigCmdArgs
        <$> pNodeConfigurationFileIn
  pTransactionView :: Parser DebugCmds
  pTransactionView =
    fmap DebugTransactionViewCmd $
      TransactionViewCmdArgs
        <$> pTxViewOutputFormat
        <*> pMaybeOutputFile
        <*> pInputTxOrTxBodyFile

pNodeConfigurationFileIn :: Parser (NodeConfigFile In)
pNodeConfigurationFileIn =
  File
    <$> parseFilePath "node-configuration-file" "Input filepath of the node configuration file."
