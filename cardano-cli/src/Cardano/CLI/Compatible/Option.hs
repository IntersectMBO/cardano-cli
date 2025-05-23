{-# LANGUAGE GADTs #-}

{-
This module is concerned with providing backwards compatible cli commands for our internal
testing needs. The intention is to restrict as much as possible which functionality we maintain backwards
compatibility for.
-}

module Cardano.CLI.Compatible.Option
  ( pAnyCompatibleCommand
  )
where

import Cardano.Api

import Cardano.CLI.Compatible.Command
import Cardano.CLI.Compatible.Governance.Option
import Cardano.CLI.Compatible.StakeAddress.Option
import Cardano.CLI.Compatible.StakePool.Option
import Cardano.CLI.Compatible.Transaction.Option
import Cardano.CLI.Environment
import Cardano.CLI.Parser

import Data.Foldable (asum)
import Data.Maybe
import Options.Applicative (Parser)
import Options.Applicative qualified as Opt

pAnyCompatibleCommand :: EnvCli -> Parser AnyCompatibleCommand
pAnyCompatibleCommand envCli =
  asum
    [ -- Note, byron is ommitted because there is already a legacy command group for it.
      Opt.hsubparser $
        commandWithMetavar "shelley" $
          Opt.info (AnyCompatibleCommand <$> pCompatibleCommand ShelleyBasedEraShelley envCli) $
            Opt.progDesc "Shelley era commands"
    , Opt.hsubparser $
        commandWithMetavar "allegra" $
          Opt.info (AnyCompatibleCommand <$> pCompatibleCommand ShelleyBasedEraAllegra envCli) $
            Opt.progDesc "Allegra era commands"
    , Opt.hsubparser $
        commandWithMetavar "mary" $
          Opt.info (AnyCompatibleCommand <$> pCompatibleCommand ShelleyBasedEraMary envCli) $
            Opt.progDesc "Mary era commands"
    , Opt.hsubparser $
        commandWithMetavar "alonzo" $
          Opt.info (AnyCompatibleCommand <$> pCompatibleCommand ShelleyBasedEraAlonzo envCli) $
            Opt.progDesc "Alonzo era commands"
    , Opt.hsubparser $
        commandWithMetavar "babbage" $
          Opt.info (AnyCompatibleCommand <$> pCompatibleCommand ShelleyBasedEraBabbage envCli) $
            Opt.progDesc "Babbage era commands"
    , Opt.hsubparser $
        commandWithMetavar "conway" $
          Opt.info (AnyCompatibleCommand <$> pCompatibleCommand ShelleyBasedEraConway envCli) $
            Opt.progDesc "Conway era commands"
    ]

pCompatibleCommand :: ShelleyBasedEra era -> EnvCli -> Parser (CompatibleCommand era)
pCompatibleCommand era env =
  asum $
    catMaybes
      [ Just $ CompatibleTransactionCmds <$> pAllCompatibleTransactionCommands env era
      , Just $ CompatibleGovernanceCmds <$> pCompatibleGovernanceCmds era
      , fmap CompatibleStakeAddressCmds <$> pCompatibleStakeAddressCmds era
      , fmap CompatibleStakePoolCmds <$> pCompatibleStakePoolCmds era env
      ]
