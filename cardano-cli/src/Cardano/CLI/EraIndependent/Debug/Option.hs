{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraIndependent.Debug.Option
  ( parseDebugCmds
  )
where

import Cardano.Api

import Cardano.CLI.Environment
import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraIndependent.Debug.CheckNodeConfiguration.Command
import Cardano.CLI.EraIndependent.Debug.CheckPoolRegistration.Command
import Cardano.CLI.EraIndependent.Debug.Command
import Cardano.CLI.EraIndependent.Debug.LogEpochState.Command
import Cardano.CLI.EraIndependent.Debug.TransactionView.Command
import Cardano.CLI.Option.Flag
import Cardano.CLI.Parser

import Data.Foldable
import Data.Function ((&))
import Options.Applicative hiding (help, str)
import Options.Applicative qualified as Opt

parseDebugCmds :: EnvCli -> Parser DebugCmds
parseDebugCmds envCli =
  Opt.hsubparser $
    mconcat
      [ Opt.metavar "debug commands"
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
        commandWithMetavar "check-pool-registration" $
          Opt.info pCheckPoolRegistrationCmdArgs $
            Opt.progDesc $
              mconcat
                [ "Compare a stake pool registration certificate that has not been submitted yet"
                , " against the pool's parameters on chain, and report what submitting it would"
                , " change."
                ]
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
  pCheckPoolRegistrationCmdArgs :: Parser DebugCmds
  pCheckPoolRegistrationCmdArgs =
    fmap DebugCheckPoolRegistrationCmd $
      CheckPoolRegistrationCmdArgs
        <$> ( LocalNodeConnectInfo
                <$> pConsensusModeParams
                <*> pNetworkId envCli
                <*> pSocketPath envCli
            )
        <*> pPoolRegistrationCertFileIn
        <*> pFormatFlags
          "check-pool-registration output"
          [ flagFormatJson
          , flagFormatText & setDefault
          , flagFormatYaml
          ]
        <*> pMaybeOutputFile
  pTransactionView :: Parser DebugCmds
  pTransactionView =
    fmap DebugTransactionViewCmd $
      TransactionViewCmdArgs
        <$> pFormatFlags
          "transaction view output"
          [ flagFormatJson & setDefault
          , flagFormatYaml
          ]
        <*> pMaybeOutputFile
        <*> pInputTxOrTxBodyFile

pPoolRegistrationCertFileIn :: Parser (File () In)
pPoolRegistrationCertFileIn =
  File
    <$> parseFilePath
      "pool-registration-cert-file"
      "Input filepath of the stake pool registration certificate to check."

pNodeConfigurationFileIn :: Parser (NodeConfigFile In)
pNodeConfigurationFileIn =
  File
    <$> parseFilePath "node-configuration-file" "Input filepath of the node configuration file."
