{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands
  ( AnyEraCommand (..)
  , EraBasedCommand (..)
  , renderAnyEraCommand
  , renderEraBasedCommand
  , pAnyEraCommand
  , pEraBasedCommand
  ) where

import           Cardano.Api (CardanoEra (..), ShelleyBasedEra (..))

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Commands.Address
import           Cardano.CLI.EraBased.Commands.StakeAddress
import           Cardano.CLI.EraBased.Commands.Transaction
import           Cardano.CLI.EraBased.Options.Address
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.EraBased.Options.Governance
import           Cardano.CLI.EraBased.Options.StakeAddress
import           Cardano.CLI.EraBased.Options.Transaction

import           Data.Foldable
import           Data.Maybe
import           Data.Text (Text)
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt

data AnyEraCommand where
  AnyEraCommandOf :: ShelleyBasedEra era -> EraBasedCommand era -> AnyEraCommand

renderAnyEraCommand :: AnyEraCommand -> Text
renderAnyEraCommand = \case
  AnyEraCommandOf _ cmd -> renderEraBasedCommand cmd

data EraBasedCommand era
  = AddressCmds (AddressCmds era)
  | EraBasedGovernanceCmds (EraBasedGovernanceCmds era)
  | TransactionCmds (TransactionCmds era)
  | StakeAddressCmds (StakeAddressCmds era)

renderEraBasedCommand :: EraBasedCommand era -> Text
renderEraBasedCommand = \case
  AddressCmds cmd ->
    renderAddressCmds cmd
  EraBasedGovernanceCmds cmd ->
    renderEraBasedGovernanceCmds cmd
  StakeAddressCmds cmd ->
    renderStakeAddressCmds cmd
  TransactionCmds cmd ->
    renderTransactionCmds cmd

pAnyEraCommand :: EnvCli -> Parser AnyEraCommand
pAnyEraCommand envCli =
  asum
    [ -- Note, byron is ommitted because there is already a legacy command group for it.

      subParser "shelley"
        $ Opt.info (AnyEraCommandOf ShelleyBasedEraShelley <$> pEraBasedCommand envCli ShelleyEra)
        $ Opt.progDesc "Shelley era commands"
    , subParser "allegra"
        $ Opt.info (AnyEraCommandOf ShelleyBasedEraAllegra <$> pEraBasedCommand envCli AllegraEra)
        $ Opt.progDesc "Allegra era commands"
    , subParser "mary"
        $ Opt.info (AnyEraCommandOf ShelleyBasedEraMary <$> pEraBasedCommand envCli MaryEra)
        $ Opt.progDesc "Mary era commands"
    , subParser "alonzo"
        $ Opt.info (AnyEraCommandOf ShelleyBasedEraAlonzo <$> pEraBasedCommand envCli AlonzoEra)
        $ Opt.progDesc "Alonzo era commands"
    , subParser "babbage"
        $ Opt.info (AnyEraCommandOf ShelleyBasedEraBabbage <$> pEraBasedCommand envCli BabbageEra)
        $ Opt.progDesc "Babbage era commands"
    , subParser "conway"
        $ Opt.info (AnyEraCommandOf ShelleyBasedEraConway <$> pEraBasedCommand envCli ConwayEra)
        $ Opt.progDesc "Conway era commands"

    , subParser "latest"
        $ Opt.info (AnyEraCommandOf ShelleyBasedEraBabbage <$> pEraBasedCommand envCli BabbageEra)
        $ Opt.progDesc "Latest era commands (Babbage)"
    ]

pEraBasedCommand :: EnvCli -> CardanoEra era -> Parser (EraBasedCommand era)
pEraBasedCommand envCli era =
  asum $ catMaybes
    [ Just
        $ subParser "address"
        $ Opt.info (AddressCmds <$> pAddressCmds era envCli)
        $ Opt.progDesc "Era-based address commands"
    , Just
        $ subParser "governance"
        $ Opt.info (EraBasedGovernanceCmds <$> pEraBasedGovernanceCmds envCli era)
        $ Opt.progDesc "Era-based governance commands"
    , fmap StakeAddressCmds <$> pStakeAddressCmds era envCli
    , Just
        $ subParser "transaction"
        $ Opt.info (TransactionCmds <$> pTransactionCmds envCli era)
        $ Opt.progDesc "Era-based governance commands"
    ]
