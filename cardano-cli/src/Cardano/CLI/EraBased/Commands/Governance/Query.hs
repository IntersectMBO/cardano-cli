{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance.Query
  ( GovernanceQueryCmds(..)
  , NoArgQueryCmd(..)
  , DRepStateQueryCmd(..)
  , DRepStakeDistributionQueryCmd(..)
  , renderGovernanceQueryCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Key

import           Data.Text (Text)

data GovernanceQueryCmds era
  = GovernanceQueryConstitutionCmd
      (ConwayEraOnwards era)
      NoArgQueryCmd
  | GovernanceQueryGovStateCmd
      (ConwayEraOnwards era)
      NoArgQueryCmd
  | GovernanceQueryDRepStateCmd
      (ConwayEraOnwards era)
      DRepStateQueryCmd
  | GovernanceQueryDRepStakeDistributionCmd
      (ConwayEraOnwards era)
      DRepStakeDistributionQueryCmd
  | GovernanceQueryCommitteeStateCmd
      (ConwayEraOnwards era)
      NoArgQueryCmd
  deriving Show

data NoArgQueryCmd = NoArgQueryCmd
  { socketPath          :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , outputFile          :: !(Maybe (File () Out))
  } deriving Show

data DRepStateQueryCmd = DRepStateQueryCmd
  { socketPath          :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , drepKeys            :: ![VerificationKeyOrHashOrFile DRepKey]
  , outputFile          :: !(Maybe (File () Out))
  } deriving Show

data DRepStakeDistributionQueryCmd = DRepStakeDistributionQueryCmd
  { socketPath          :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , drepKeys            :: ![VerificationKeyOrHashOrFile DRepKey]
  , outputFile          :: !(Maybe (File () Out))
  } deriving Show

renderGovernanceQueryCmds :: GovernanceQueryCmds era -> Text
renderGovernanceQueryCmds = ("governance query " <>) . \case
  GovernanceQueryConstitutionCmd{}          -> "constitution"
  GovernanceQueryGovStateCmd{}              -> "gov-state"
  GovernanceQueryDRepStateCmd{}             -> "drep-state"
  GovernanceQueryDRepStakeDistributionCmd{} -> "drep-stake-distribution"
  GovernanceQueryCommitteeStateCmd{}        -> "committee-state"
