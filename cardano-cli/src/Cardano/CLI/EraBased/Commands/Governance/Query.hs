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
  = GovernanceQueryConstitutionCmd            (NoArgQueryCmd era)
  | GovernanceQueryGovStateCmd                (NoArgQueryCmd era)
  | GovernanceQueryDRepStateCmd               (DRepStateQueryCmd era)
  | GovernanceQueryDRepStakeDistributionCmd   (DRepStakeDistributionQueryCmd era)
  | GovernanceQueryCommitteeStateCmd          (NoArgQueryCmd era)
  deriving Show

data NoArgQueryCmd era = NoArgQueryCmd
  { cmdEon              :: ConwayEraOnwards era
  , socketPath          :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , outputFile          :: !(Maybe (File () Out))
  } deriving Show

data DRepStateQueryCmd era = DRepStateQueryCmd
  { cmdEon              :: ConwayEraOnwards era
  , socketPath          :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , drepKeys            :: ![VerificationKeyOrHashOrFile DRepKey]
  , outputFile          :: !(Maybe (File () Out))
  } deriving Show

data DRepStakeDistributionQueryCmd era = DRepStakeDistributionQueryCmd
  { cmdEon              :: ConwayEraOnwards era
  , socketPath          :: !SocketPath
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
