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
    [ pGovernanceQueryGetConstitutionCmd env era
    , pGovernanceQueryGetGovStateCmd env era
    , pGovernanceQueryDRepStateCmd env era
    , pGovernanceQueryDRepStakeDistributionCmd env era
    , pGovernanceQueryGetCommitteeStateCmd env era
    ]

pGovernanceQueryGetConstitutionCmd
  :: EnvCli
  -> CardanoEra era
  -> Maybe (Parser (GovernanceQueryCmds era))
pGovernanceQueryGetConstitutionCmd env era = do
  cOn <- maybeFeatureInEra era
  pure
    $ subParser "constitution"
    $ Opt.info (GovernanceQueryConstitutionCmd cOn <$> pNoArgQueryCmd env)
    $ Opt.progDesc "Get the constitution"

pGovernanceQueryGetGovStateCmd
  :: EnvCli
  -> CardanoEra era
  -> Maybe (Parser (GovernanceQueryCmds era))
pGovernanceQueryGetGovStateCmd env era = do
  cOn <- maybeFeatureInEra era
  pure
    $ subParser "gov-state"
    $ Opt.info (GovernanceQueryGovStateCmd cOn <$> pNoArgQueryCmd env)
    $ Opt.progDesc "Get the governance state"

-- TODO Conway: DRep State and DRep Stake Distribution parsers use DRep keys to obtain DRep credentials. This only
-- makes use of 'KeyHashObj' constructor of 'Credential kr c'. Should we also support here 'ScriptHashObj'?
-- What about 'DRep c' - this means that only 'KeyHash' constructor is in use here: should also
-- 'DRepAlwaysAbstain' and 'DRepAlwaysNoConfidence' be supported here?

pGovernanceQueryDRepStateCmd
  :: EnvCli
  -> CardanoEra era
  -> Maybe (Parser (GovernanceQueryCmds era))
pGovernanceQueryDRepStateCmd env era = do
  cOn <- maybeFeatureInEra era
  pure
    $ subParser "drep-state"
    $ Opt.info (GovernanceQueryDRepStateCmd cOn <$> pDRepStateQueryCmd)
    $ Opt.progDesc "Get the DRep state"
  where
    pDRepStateQueryCmd :: Parser DRepStateQueryCmd
    pDRepStateQueryCmd = DRepStateQueryCmd
      <$> pSocketPath env
      <*> pConsensusModeParams
      <*> pNetworkId env
      <*> some pDRepVerificationKeyOrHashOrFile
      <*> optional pOutputFile

pGovernanceQueryDRepStakeDistributionCmd
  :: EnvCli
  -> CardanoEra era
  -> Maybe (Parser (GovernanceQueryCmds era))
pGovernanceQueryDRepStakeDistributionCmd env era = do
  cOn <- maybeFeatureInEra era
  pure
    $ subParser "drep-stake-distribution"
    $ Opt.info (GovernanceQueryDRepStakeDistributionCmd cOn <$> pDRepStakeDistributionQueryCmd)
    $ Opt.progDesc "Get the DRep stake distribution"
  where
    pDRepStakeDistributionQueryCmd :: Parser DRepStakeDistributionQueryCmd
    pDRepStakeDistributionQueryCmd = DRepStakeDistributionQueryCmd
      <$> pSocketPath env
      <*> pConsensusModeParams
      <*> pNetworkId env
      <*> some pDRepVerificationKeyOrHashOrFile
      <*> optional pOutputFile

pGovernanceQueryGetCommitteeStateCmd
  :: EnvCli
  -> CardanoEra era
  -> Maybe (Parser (GovernanceQueryCmds era))
pGovernanceQueryGetCommitteeStateCmd env era = do
  cOn <- maybeFeatureInEra era
  pure
    $ subParser "committee-state"
    $ Opt.info (GovernanceQueryCommitteeStateCmd cOn <$> pNoArgQueryCmd env)
    $ Opt.progDesc "Get the committee state"

pNoArgQueryCmd :: EnvCli -> Parser NoArgQueryCmd
pNoArgQueryCmd env =
  NoArgQueryCmd
    <$> pSocketPath env
    <*> pConsensusModeParams
    <*> pNetworkId env
    <*> optional pOutputFile
