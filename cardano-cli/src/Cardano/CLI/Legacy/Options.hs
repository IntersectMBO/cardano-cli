{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

module Cardano.CLI.Legacy.Options
  ( parseLegacyCmds
  ) where

import           Cardano.CLI.Environment (EnvCli (..))
import           Cardano.CLI.Legacy.Commands
import           Cardano.CLI.Legacy.Options.Address
import           Cardano.CLI.Legacy.Options.Genesis
import           Cardano.CLI.Legacy.Options.Governance
import           Cardano.CLI.Legacy.Options.Key
import           Cardano.CLI.Legacy.Options.Node
import           Cardano.CLI.Legacy.Options.Pool
import           Cardano.CLI.Legacy.Options.Query
import           Cardano.CLI.Legacy.Options.StakeAddress
import           Cardano.CLI.Legacy.Options.TextView
import           Cardano.CLI.Legacy.Options.Transaction

import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

parseLegacyCmds :: EnvCli -> Parser LegacyCmds
parseLegacyCmds envCli =
  Opt.hsubparser $ mconcat
    [ Opt.metavar "Legacy commands"
    , Opt.commandGroup "Legacy commands"
    , Opt.command "address"
        $ Opt.info (LegacyAddressCmds <$> parseAddressCmds envCli)
        $ Opt.progDesc "Payment address commands"
    , Opt.command "stake-address"
        $ Opt.info (LegacyStakeAddressCmds <$> parseStakeAddressCmds envCli)
        $ Opt.progDesc "Stake address commands"
    , Opt.command "key"
        $ Opt.info (LegacyKeyCmds <$> parseKeyCmds)
        $ Opt.progDesc "Key utility commands"
    , Opt.command "transaction"
        $ Opt.info (LegacyTransactionCmds <$> parseTransactionCmds envCli)
        $ Opt.progDesc "Transaction commands"
    , Opt.command "node"
        $ Opt.info (LegacyNodeCmds <$> parseNodeCmds)
        $ Opt.progDesc "Node operation commands"
    , Opt.command "stake-pool"
        $ Opt.info (LegacyPoolCmds <$> parsePoolCmds envCli)
        $ Opt.progDesc "Stake pool commands"
    , Opt.command "query"
        $ Opt.info (LegacyQueryCmds <$> parseQueryCmds envCli) . Opt.progDesc
        $ mconcat
            [ "Node query commands. Will query the local node whose Unix domain socket "
            , "is obtained from the CARDANO_NODE_SOCKET_PATH environment variable."
            ]
    , Opt.command "genesis"
        $ Opt.info (LegacyGenesisCmds <$> parseGenesisCmds envCli)
        $ Opt.progDesc "Genesis block commands"
    , Opt.command "governance"
        $ Opt.info (LegacyGovernanceCmds <$> parseGovernanceCmds envCli)
        $ Opt.progDesc "Governance commands"
    , Opt.command "text-view"
        $ Opt.info (LegacyTextViewCmds <$> parseTextViewCmds) . Opt.progDesc
        $ mconcat
            [ "Commands for dealing with Shelley TextView files. "
            , "Transactions, addresses etc are stored on disk as TextView files."
            ]
    ]
