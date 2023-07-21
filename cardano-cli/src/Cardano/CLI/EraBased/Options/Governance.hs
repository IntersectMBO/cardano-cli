{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Options.Governance where

import           Cardano.Api

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Legacy
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Types.Key

import           Data.Foldable
import           Data.Maybe
import           Data.Text (Text)
import           Options.Applicative
import qualified Options.Applicative as Opt

data EraBasedGovernanceCmd era
  = EraBasedGovernancePreConwayCmd (ShelleyToBabbageEra era)
  | EraBasedGovernancePostConwayCmd (ConwayEraOnwards era)
  | EraBasedGovernanceDelegationCertificateCmd AnyDelegationTarget

renderEraBasedGovernanceCmd :: EraBasedGovernanceCmd era -> Text
renderEraBasedGovernanceCmd = \case
  EraBasedGovernancePreConwayCmd {} -> "governance pre-conway"
  EraBasedGovernancePostConwayCmd {} -> "governance post-conway"

  EraBasedGovernanceDelegationCertificateCmd {} -> "governance delegation-certificate"
-- TODO: Conway era - move to Cardano.CLI.Conway.Parsers
pEraBasedGovernanceCmd :: EnvCli -> CardanoEra era -> Parser (EraBasedGovernanceCmd era)
pEraBasedGovernanceCmd envCli era =
  asum $ catMaybes
    [ pEraBasedDelegationCertificateCmd envCli era
    ]

data AnyEraDecider era where
  AnyEraDeciderShelleyToBabbage :: ShelleyToBabbageEra era -> AnyEraDecider era
  AnyEraDeciderConwayOnwards :: ConwayEraOnwards era -> AnyEraDecider era

instance FeatureInEra AnyEraDecider where
  featureInEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraShelley
    AllegraEra  -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraAllegra
    MaryEra     -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraMary
    AlonzoEra   -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraAlonzo
    BabbageEra  -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraBabbage
    ConwayEra   -> yes $ AnyEraDeciderConwayOnwards ConwayEraOnwardsConway

pEraBasedDelegationCertificateCmd :: EnvCli -> CardanoEra era -> Maybe (Parser (EraBasedGovernanceCmd era))
pEraBasedDelegationCertificateCmd _envCli =
  featureInEra Nothing $ \f ->
        Just
          $ fmap EraBasedGovernanceDelegationCertificateCmd
          $ subParser "delegation-certificate"
          $ Opt.info (pAnyDelegationCertificateTarget f)
          $ Opt.progDesc "Post conway era governance command" -- TODO: We can render the help message based on the era
 where
  pAnyDelegationCertificateTarget :: AnyEraDecider era -> Parser AnyDelegationTarget
  pAnyDelegationCertificateTarget e =
    case e of
      AnyEraDeciderShelleyToBabbage sbe ->
        ShelleyToBabbageDelegTarget sbe
          <$> pStakeIdentifier
          <*> pStakePoolVerificationKeyOrHashOrFile
      AnyEraDeciderConwayOnwards cOnwards ->
        ConwayOnwardDelegTarget cOnwards
          <$> pStakeIdentifier
          <*> pStakeTarget cOnwards

pStakeTarget :: ConwayEraOnwards era -> Parser (StakeTarget era)
pStakeTarget cOnwards =
  asum
    [ TargetStakePool cOnwards <$> pStakePoolVerificationKeyOrHashOrFile
    , TargetVotingDrep cOnwards <$ pDrep
    -- , TargetVotingDrepAndStakePool cOnwards -- TODO: Conway era
    ]

{-
data DRep c
  = DRepKeyHash !(KeyHash 'Voting c)
  | DRepScriptHash !(ScriptHash c)
  | DRepAlwaysAbstain
  | DRepAlwaysNoConfidence
-}

-- TODO: Conway era - parse the relevant voting
-- credential (key hash, script hash, always abstain or no confidence)
pDrep :: Parser String
pDrep = Opt.strOption $ mconcat
          [ Opt.long "dummy-drep-option"
          , Opt.help "Delegate voting stake to Drep"
          ]



pPreConwayCmd :: EnvCli -> CardanoEra era -> Maybe (Parser (EraBasedGovernanceCmd era))
pPreConwayCmd envCli =
  featureInEra Nothing $ \feature ->
    Just
      $ subParser "pre-conway"
      $ Opt.info (pPreConwayArgs envCli feature)
      $ Opt.progDesc "Pre conway era governance command"

pPreConwayArgs :: EnvCli -> ShelleyToBabbageEra era -> Parser (EraBasedGovernanceCmd era)
pPreConwayArgs _envCli w = pure (EraBasedGovernancePreConwayCmd w)

pPostConwayArgs :: EnvCli -> ConwayEraOnwards era -> Parser (EraBasedGovernanceCmd era)
pPostConwayArgs _envCli w = pure (EraBasedGovernancePostConwayCmd w)
