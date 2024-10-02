{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

{-
This module is concerned with providing backwards compatible cli commands for our internal
testing needs. The intention is to restrict as much as possible which functionality we maintain backwards
compatibility for.
-}

module Cardano.CLI.Compatible.Commands
  ( AnyCompatibleCommand (..)
  , CompatibleCommand (..)
  , pAnyCompatibleCommand
  , renderAnyCompatibleCommand
  )
where

import           Cardano.Api

import           Cardano.CLI.Compatible.Governance
import           Cardano.CLI.Compatible.Transaction
import           Cardano.CLI.Environment
import           Cardano.CLI.Parser

import           Data.Foldable
import           Data.Text
import           Options.Applicative
import qualified Options.Applicative as Opt

data AnyCompatibleCommand where
  AnyCompatibleCommand :: CompatibleCommand era -> AnyCompatibleCommand

renderAnyCompatibleCommand :: AnyCompatibleCommand -> Text
renderAnyCompatibleCommand = \case
  AnyCompatibleCommand cmd -> renderCompatibleCommand cmd

data CompatibleCommand era
  = CompatibleTransactionCmd (CompatibleTransactionCmds era)
  | CompatibleGovernanceCmds (CompatibleGovernanceCmds era)

renderCompatibleCommand :: CompatibleCommand era -> Text
renderCompatibleCommand = \case
  CompatibleTransactionCmd cmd -> renderCompatibleTransactionCmd cmd
  CompatibleGovernanceCmds cmd -> renderCompatibleGovernanceCmds cmd

pAnyCompatibleCommand :: EnvCli -> Parser AnyCompatibleCommand
pAnyCompatibleCommand envCli =
  asum
    [ -- Note, byron is ommitted because there is already a legacy command group for it.
      subParser "shelley" $
        Opt.info (AnyCompatibleCommand <$> pCompatibleCommand ShelleyBasedEraShelley envCli) $
          Opt.progDesc "Shelley era commands"
    , subParser "allegra" $
        Opt.info (AnyCompatibleCommand <$> pCompatibleCommand ShelleyBasedEraAllegra envCli) $
          Opt.progDesc "Allegra era commands"
    , subParser "mary" $
        Opt.info (AnyCompatibleCommand <$> pCompatibleCommand ShelleyBasedEraMary envCli) $
          Opt.progDesc "Mary era commands"
    , subParser "alonzo" $
        Opt.info (AnyCompatibleCommand <$> pCompatibleCommand ShelleyBasedEraAlonzo envCli) $
          Opt.progDesc "Alonzo era commands"
    , subParser "babbage" $
        Opt.info (AnyCompatibleCommand <$> pCompatibleCommand ShelleyBasedEraBabbage envCli) $
          Opt.progDesc "Babbage era commands"
    , subParser "conway" $
        Opt.info (AnyCompatibleCommand <$> pCompatibleCommand ShelleyBasedEraConway envCli) $
          Opt.progDesc "Conway era commands"
    ]

pCompatibleCommand :: ShelleyBasedEra era -> EnvCli -> Parser (CompatibleCommand era)
pCompatibleCommand era env =
  asum
    [ CompatibleTransactionCmd <$> pAllCompatibleTransactionCommands env era
    , CompatibleGovernanceCmds <$> pCompatibleGovernanceCmds era
    ]
