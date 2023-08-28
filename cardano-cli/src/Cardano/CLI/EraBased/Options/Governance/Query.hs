{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Options.Governance.Query
  ( pGovernanceQueryCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Commands.Governance.Query
import           Cardano.CLI.EraBased.Options.Common

import           Options.Applicative
import qualified Options.Applicative as Opt

pGovernanceQueryCmds
  :: ()
  => EnvCli
  -> CardanoEra era
  -> Maybe (Parser (GovernanceQueryCmds era))
pGovernanceQueryCmds env era =
  subInfoParser "query"
    ( Opt.progDesc "Query governance-related information" )
    [ pGovernanceQueryGetConstitution env era
    , pGovernanceQueryGetGovState env era
    , pGovernanceQueryDRepState env era
    , pGovernanceQueryDRepStakeDistribution env era
    , pGovernanceQueryGetCommitteeState env era
    ]

pGovernanceQueryGetConstitution
  :: EnvCli
  -> CardanoEra era
  -> Maybe (Parser (GovernanceQueryCmds era))
pGovernanceQueryGetConstitution env era = do
  cOn <- maybeFeatureInEra era
  pure
    $ subParser "constitution"
    $ Opt.info (GovernanceQueryConstitution cOn <$> pEraBasedNoArgQuery env)
    $ Opt.progDesc "Get the constitution"

pGovernanceQueryGetGovState
  :: EnvCli
  -> CardanoEra era
  -> Maybe (Parser (GovernanceQueryCmds era))
pGovernanceQueryGetGovState env era = do
  cOn <- maybeFeatureInEra era
  pure
    $ subParser "gov-state"
    $ Opt.info (GovernanceQueryGovState cOn <$> pEraBasedNoArgQuery env)
    $ Opt.progDesc "Get the governance state"

-- TODO Conway: DRep State and DRep Stake Distribution parsers use DRep keys to obtain DRep credentials. This only
-- makes use of 'KeyHashObj' constructor of 'Credential kr c'. Should we also support here 'ScriptHashObj'?
-- What about 'DRep c' - this means that only 'KeyHash' constructor is in use here: should also
-- 'DRepAlwaysAbstain' and 'DRepAlwaysNoConfidence' be supported here?

pGovernanceQueryDRepState
  :: EnvCli
  -> CardanoEra era
  -> Maybe (Parser (GovernanceQueryCmds era))
pGovernanceQueryDRepState env era = do
  cOn <- maybeFeatureInEra era
  pure
    $ subParser "drep-state"
    $ Opt.info (GovernanceQueryDRepState cOn <$> pEraBasedDRepStateQuery)
    $ Opt.progDesc "Get the DRep state"
  where
    pEraBasedDRepStateQuery :: Parser EraBasedDRepStateQuery
    pEraBasedDRepStateQuery = EraBasedDRepStateQuery
      <$> pSocketPath env
      <*> pConsensusModeParams
      <*> pNetworkId env
      <*> some pDRepVerificationKeyOrHashOrFile
      <*> optional pOutputFile

pGovernanceQueryDRepStakeDistribution
  :: EnvCli
  -> CardanoEra era
  -> Maybe (Parser (GovernanceQueryCmds era))
pGovernanceQueryDRepStakeDistribution env era = do
  cOn <- maybeFeatureInEra era
  pure
    $ subParser "drep-stake-distribution"
    $ Opt.info (GovernanceQueryDRepStakeDistribution cOn <$> pEraBasedDRepStakeDistributionQuery)
    $ Opt.progDesc "Get the DRep stake distribution"
  where
    pEraBasedDRepStakeDistributionQuery :: Parser EraBasedDRepStakeDistributionQuery
    pEraBasedDRepStakeDistributionQuery = EraBasedDRepStakeDistributionQuery
      <$> pSocketPath env
      <*> pConsensusModeParams
      <*> pNetworkId env
      <*> some pDRepVerificationKeyOrHashOrFile
      <*> optional pOutputFile

pGovernanceQueryGetCommitteeState
  :: EnvCli
  -> CardanoEra era
  -> Maybe (Parser (GovernanceQueryCmds era))
pGovernanceQueryGetCommitteeState env era = do
  cOn <- maybeFeatureInEra era
  pure
    $ subParser "committee-state"
    $ Opt.info (GovernanceQueryCommitteeState cOn <$> pEraBasedNoArgQuery env)
    $ Opt.progDesc "Get the committee state"

pEraBasedNoArgQuery :: EnvCli -> Parser EraBasedNoArgQuery
pEraBasedNoArgQuery env =
  EraBasedNoArgQuery
    <$> pSocketPath env
    <*> pConsensusModeParams
    <*> pNetworkId env
    <*> optional pOutputFile

