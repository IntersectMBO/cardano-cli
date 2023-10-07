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

pGovernanceQueryCmds :: ()
  => CardanoEra era
  -> EnvCli
  -> Maybe (Parser (GovernanceQueryCmds era))
pGovernanceQueryCmds era env =
  subInfoParser "query"
    ( Opt.progDesc "Query governance-related information" )
    [ pGovernanceQueryGetConstitutionCmd era env
    , pGovernanceQueryGetGovStateCmd era env
    , pGovernanceQueryDRepStateCmd era env
    , pGovernanceQueryDRepStakeDistributionCmd era env
    , pGovernanceQueryGetCommitteeStateCmd era env
    ]

pGovernanceQueryGetConstitutionCmd :: ()
  => CardanoEra era
  -> EnvCli
  -> Maybe (Parser (GovernanceQueryCmds era))
pGovernanceQueryGetConstitutionCmd era env = do
  w <- forEraMaybeEon era
  pure
    $ subParser "constitution"
    $ Opt.info (GovernanceQueryConstitutionCmd <$> pNoArgQueryCmd w env)
    $ Opt.progDesc "Get the constitution"

pGovernanceQueryGetGovStateCmd :: ()
  => CardanoEra era
  -> EnvCli
  -> Maybe (Parser (GovernanceQueryCmds era))
pGovernanceQueryGetGovStateCmd era env = do
  w <- forEraMaybeEon era
  pure
    $ subParser "gov-state"
    $ Opt.info (GovernanceQueryGovStateCmd <$> pNoArgQueryCmd w env)
    $ Opt.progDesc "Get the governance state"

-- TODO Conway: DRep State and DRep Stake Distribution parsers use DRep keys to obtain DRep credentials. This only
-- makes use of 'KeyHashObj' constructor of 'Credential kr c'. Should we also support here 'ScriptHashObj'?
-- What about 'DRep c' - this means that only 'KeyHash' constructor is in use here: should also
-- 'DRepAlwaysAbstain' and 'DRepAlwaysNoConfidence' be supported here?

pGovernanceQueryDRepStateCmd :: ()
  => CardanoEra era
  -> EnvCli
  -> Maybe (Parser (GovernanceQueryCmds era))
pGovernanceQueryDRepStateCmd era env = do
  w <- forEraMaybeEon era
  pure
    $ subParser "drep-state"
    $ Opt.info (GovernanceQueryDRepStateCmd <$> pDRepStateQueryCmd w)
    $ Opt.progDesc "Get the DRep state. If no DRep credentials are provided, return states for all of them."
  where
    pDRepStateQueryCmd :: ConwayEraOnwards era -> Parser (DRepStateQueryCmd era)
    pDRepStateQueryCmd w =
      DRepStateQueryCmd w
        <$> pSocketPath env
        <*> pConsensusModeParams
        <*> pNetworkId env
        <*> some pDRepVerificationKeyOrHashOrFile
        <*> optional pOutputFile

pGovernanceQueryDRepStakeDistributionCmd :: ()
  => CardanoEra era
  -> EnvCli
  -> Maybe (Parser (GovernanceQueryCmds era))
pGovernanceQueryDRepStakeDistributionCmd era env = do
  w <- forEraMaybeEon era
  pure
    $ subParser "drep-stake-distribution"
    $ Opt.info (GovernanceQueryDRepStakeDistributionCmd <$> pDRepStakeDistributionQueryCmd w)
    $ Opt.progDesc "Get the DRep stake distribution. If no DRep credentials are provided, return stake distributions for all of them."
  where
    pDRepStakeDistributionQueryCmd :: ConwayEraOnwards era -> Parser (DRepStakeDistributionQueryCmd era)
    pDRepStakeDistributionQueryCmd w = DRepStakeDistributionQueryCmd w
      <$> pSocketPath env
      <*> pConsensusModeParams
      <*> pNetworkId env
      <*> some pDRepVerificationKeyOrHashOrFile
      <*> optional pOutputFile

pGovernanceQueryGetCommitteeStateCmd :: ()
  => CardanoEra era
  -> EnvCli
  -> Maybe (Parser (GovernanceQueryCmds era))
pGovernanceQueryGetCommitteeStateCmd era env = do
  w <- forEraMaybeEon era
  pure
    $ subParser "committee-state"
    $ Opt.info (GovernanceQueryCommitteeStateCmd <$> pNoArgQueryCmd w env)
    $ Opt.progDesc "Get the committee state"

pNoArgQueryCmd :: ()
  => ConwayEraOnwards era
  -> EnvCli
  -> Parser (NoArgQueryCmd era)
pNoArgQueryCmd w env =
  NoArgQueryCmd w
    <$> pSocketPath env
    <*> pConsensusModeParams
    <*> pNetworkId env
    <*> optional pOutputFile
