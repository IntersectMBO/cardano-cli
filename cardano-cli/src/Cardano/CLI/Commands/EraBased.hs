{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Commands.EraBased
  ( EraBasedCommand (..)
  , AnyEraCommand (..)
  , renderEraBasedCommand
  , pAnyEraCommand
  , pEraBasedCommand
  ) where

import           Cardano.Api (CardanoEra (..))

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.EraBased.Options.Governance

import           Data.Foldable
import           Data.Text (Text)
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt

data AnyEraCommand where
  AnyEraCommandOf :: CardanoEra era -> EraBasedCommand era -> AnyEraCommand

newtype EraBasedCommand era
  = EraBasedGovernanceCmd (EraBasedGovernanceCmd era)

renderEraBasedCommand :: EraBasedCommand era -> Text
renderEraBasedCommand = \case
  EraBasedGovernanceCmd cmd -> renderEraBasedGovernanceCmd cmd

pAnyEraCommand :: EnvCli -> Parser AnyEraCommand
pAnyEraCommand envCli =
  asum
    [ -- Note, byron is ommitted because there is already a legacy command group for it.

      subParser "shelley"
        $ Opt.info (AnyEraCommandOf ShelleyEra <$> pEraBasedCommand envCli ShelleyEra)
        $ Opt.progDesc "Shelley era commands"
    , subParser "allegra"
        $ Opt.info (AnyEraCommandOf AllegraEra <$> pEraBasedCommand envCli AllegraEra)
        $ Opt.progDesc "Allegra era commands"
    , subParser "mary"
        $ Opt.info (AnyEraCommandOf MaryEra <$> pEraBasedCommand envCli MaryEra)
        $ Opt.progDesc "Mary era commands"
    , subParser "alonzo"
        $ Opt.info (AnyEraCommandOf AlonzoEra <$> pEraBasedCommand envCli AlonzoEra)
        $ Opt.progDesc "Alonzo era commands"
    , subParser "babbage"
        $ Opt.info (AnyEraCommandOf BabbageEra <$> pEraBasedCommand envCli BabbageEra)
        $ Opt.progDesc "Babbage era commands"
    , subParser "conway"
        $ Opt.info (AnyEraCommandOf ConwayEra <$> pEraBasedCommand envCli ConwayEra)
        $ Opt.progDesc "Conway era commands"

    , subParser "latest"
        $ Opt.info (AnyEraCommandOf BabbageEra <$> pEraBasedCommand envCli BabbageEra)
        $ Opt.progDesc "Latest era commands (Babbage)"
    , subParser "experimental"
        $ Opt.info (AnyEraCommandOf ConwayEra <$> pEraBasedCommand envCli ConwayEra)
        $ Opt.progDesc "Experimental era commands (Conway)"
    ]

pEraBasedCommand :: EnvCli -> CardanoEra era -> Parser (EraBasedCommand era)
pEraBasedCommand envCli era =
  asum
    [ subParser "governance"
        $ Opt.info (EraBasedGovernanceCmd <$> pEraBasedGovernanceCmd envCli era)
        $ Opt.progDesc "Era-based governance commands"
    ]
