{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

module Cardano.CLI.Options.Debug
  ( parseDebugCmds
  )
where

import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..), parseFilePath)

import           Cardano.CLI.Commands.Debug
import           Cardano.CLI.Commands.Debug.LogEpochState
import           Cardano.CLI.Commands.Debug.TransactionView
import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Options.Common

import           Data.Foldable
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

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
    [ subParser "log-epoch-state" $
        Opt.info pLogEpochStateCmdArgs $
          Opt.progDesc $
            mconcat
              [ "Log epoch state of a running node."
              , " This command will connect to a local node and log the epoch state to a file."
              , " The log file format is line delimited JSON."
              , " The command will not terminate."
              ]
    , subParser "transaction" $
        Opt.info
          ( asum
              [ subParser "view" (Opt.info pTransactionView $ Opt.progDesc "Print a transaction.")
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
