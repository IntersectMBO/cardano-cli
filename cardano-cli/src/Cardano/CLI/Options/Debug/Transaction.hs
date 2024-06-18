{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

module Cardano.CLI.Options.Debug.Transaction
  ( parseDebugCmds
  )
where

import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.Commands.Debug
import           Cardano.CLI.Commands.Debug.LogEpochState
import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Options.Debug.Transaction.Echo

import           Data.Foldable
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

parseDebugCmds :: EnvCli -> Parser DebugCmds
parseDebugCmds envCli =
  Opt.hsubparser $
    mconcat
      [ Opt.metavar "debug transaction commands"
      , Opt.commandGroup "debug commands"
      , Opt.command "transaction" $
          Opt.info (pDebugCmds envCli) $
            Opt.progDesc "Debug transaction commands"
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
    , subParser "echo" $
        Opt.info (DebugTransactionCmds <$> pDebugTransactionEcho) $
          Opt.progDesc "Echo a transaction"
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

pNodeConfigurationFileIn :: Parser (NodeConfigFile In)
pNodeConfigurationFileIn =
  fmap File $
    Opt.strOption $
      mconcat
        [ Opt.long "node-configuration-file"
        , Opt.metavar "FILE"
        , Opt.help "Input filepath of the node configuration file."
        , Opt.completer (Opt.bashCompleter "file")
        ]
