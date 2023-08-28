{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance.Query
  ( GovernanceQueryCmds(..)
  , EraBasedNoArgQuery(..)
  , EraBasedDRepStateQuery(..)
  , EraBasedDRepStakeDistributionQuery(..)
  , renderGovernanceQueryCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Key

import           Data.Text (Text)

data GovernanceQueryCmds era
  = GovernanceQueryConstitution
      (ConwayEraOnwards era)
      EraBasedNoArgQuery
  | GovernanceQueryGovState
      (ConwayEraOnwards era)
      EraBasedNoArgQuery
  | GovernanceQueryDRepState
      (ConwayEraOnwards era)
      EraBasedDRepStateQuery
  | GovernanceQueryDRepStakeDistribution
      (ConwayEraOnwards era)
      EraBasedDRepStakeDistributionQuery
  | GovernanceQueryCommitteeState
      (ConwayEraOnwards era)
      EraBasedNoArgQuery
  deriving Show

data EraBasedNoArgQuery = EraBasedNoArgQuery
  { naSocketPath          :: !SocketPath
  , naConsensusModeParams :: !AnyConsensusModeParams
  , naNetworkId           :: !NetworkId
  , naOutputFile          :: !(Maybe (File () Out))
  } deriving Show

data EraBasedDRepStateQuery = EraBasedDRepStateQuery
  { dsSocketPath          :: !SocketPath
  , dsConsensusModeParams :: !AnyConsensusModeParams
  , dsNetworkId           :: !NetworkId
  , dsDRepKeys            :: ![VerificationKeyOrHashOrFile DRepKey]
  , dsOutputFile          :: !(Maybe (File () Out))
  } deriving Show

data EraBasedDRepStakeDistributionQuery = EraBasedDRepStakeDistributionQuery
  { dsdSocketPath          :: !SocketPath
  , dsdConsensusModeParams :: !AnyConsensusModeParams
  , dsdNetworkId           :: !NetworkId
  , dsdDRepKeys            :: ![VerificationKeyOrHashOrFile DRepKey]
  , dsdOutputFile          :: !(Maybe (File () Out))
  } deriving Show

renderGovernanceQueryCmds :: GovernanceQueryCmds era -> Text
renderGovernanceQueryCmds = ("governance query " <>) . \case
  GovernanceQueryConstitution{}          -> "constitution"
  GovernanceQueryGovState{}              -> "gov-state"
  GovernanceQueryDRepState{}             -> "drep-state"
  GovernanceQueryDRepStakeDistribution{} -> "drep-stake-distribution"
  GovernanceQueryCommitteeState{}        -> "committee-state"
