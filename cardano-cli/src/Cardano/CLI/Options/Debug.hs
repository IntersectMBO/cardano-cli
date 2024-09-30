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
import           Cardano.CLI.Commands.Debug.CheckNodeConfiguration
import           Cardano.CLI.Commands.Debug.LogEpochState
import           Cardano.CLI.Commands.Debug.TransactionView
import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Types.Common (FixNodeConfigFile (..))

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
    , subParser "check-node-configuration" $
        Opt.info pCheckNodeConfigurationCmdArgs $
          Opt.progDesc
            "Check hashes and paths of genesis files in the given node configuration file."
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
  pCheckNodeConfigurationCmdArgs :: Parser DebugCmds
  pCheckNodeConfigurationCmdArgs =
    fmap DebugCheckNodeConfigurationCmd $
      CheckNodeConfigCmdArgs
        <$> pNodeConfigurationFileIn
        <*> pFixConfigurationFile
  pTransactionView :: Parser DebugCmds
  pTransactionView =
    fmap DebugTransactionViewCmd $
      TransactionViewCmdArgs
        <$> pTxViewOutputFormat
        <*> pMaybeOutputFile
        <*> pInputTxOrTxBodyFile
  pFixConfigurationFile :: Parser FixNodeConfigFile
  pFixConfigurationFile =
    flag
      DontFix -- By default, do not fix the file
      FixInPlace -- When flagged is passed, fix the file
      ( long "fix"
          <> ( Opt.help $
                mconcat
                  [ "Modify the input file in place, fixing wrong genesis hashes (if any)."
                  , " This may change the file's formatting."
                  ]
             )
      )

pNodeConfigurationFileIn :: Parser (NodeConfigFile In)
pNodeConfigurationFileIn =
  File
    <$> parseFilePath "node-configuration-file" "Input filepath of the node configuration file."
