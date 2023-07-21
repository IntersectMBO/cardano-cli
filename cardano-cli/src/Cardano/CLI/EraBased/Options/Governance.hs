{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Options.Governance where

import           Cardano.Api (CardanoEra (..), FeatureInEra (..))

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Types.Era

import           Data.Foldable
import           Data.Maybe
import           Data.Text (Text)
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt

data EraBasedGovernanceCmd era
  = EraBasedGovernancePreConwayCmd (ShelleyToBabbageEra era)
  | EraBasedGovernancePostConwayCmd (ConwayEraOnwards era)

renderEraBasedGovernanceCmd :: EraBasedGovernanceCmd era -> Text
renderEraBasedGovernanceCmd = \case
  EraBasedGovernancePreConwayCmd {} -> "governance pre-conway"
  EraBasedGovernancePostConwayCmd {} -> "governance post-conway"

-- TODO: Conway era - move to Cardano.CLI.Conway.Parsers
pEraBasedGovernanceCmd :: EnvCli -> CardanoEra era -> Parser (EraBasedGovernanceCmd era)
pEraBasedGovernanceCmd envCli era =
  asum $ catMaybes
    [ pPostConwayCmd envCli era
    , pPreConwayCmd envCli era
    ]

pPreConwayCmd :: EnvCli -> CardanoEra era -> Maybe (Parser (EraBasedGovernanceCmd era))
pPreConwayCmd envCli =
  featureInEra Nothing $ \feature ->
    Just
      $ subParser "pre-conway"
      $ Opt.info (pPreConwayArgs envCli feature)
      $ Opt.progDesc "Pre conway era governance command"

pPostConwayCmd :: EnvCli -> CardanoEra era -> Maybe (Parser (EraBasedGovernanceCmd era))
pPostConwayCmd envCli =
  featureInEra Nothing $ \feature ->
    Just
      $ subParser "post-conway"
      $ Opt.info (pPostConwayArgs envCli feature)
      $ Opt.progDesc "Post conway era governance command"


pPreConwayArgs :: EnvCli -> ShelleyToBabbageEra era -> Parser (EraBasedGovernanceCmd era)
pPreConwayArgs _envCli w = pure (EraBasedGovernancePreConwayCmd w)

pPostConwayArgs :: EnvCli -> ConwayEraOnwards era -> Parser (EraBasedGovernanceCmd era)
pPostConwayArgs _envCli w = pure (EraBasedGovernancePostConwayCmd w)
