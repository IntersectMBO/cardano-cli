{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance.Query
  ( GovernanceQueryCmds(..)
  , QueryDRepStakeDistributionCmdArgs(..)
  , QueryDRepStateCmdArgs(..)
  , QueryNoArgCmdArgs(..)
  , renderGovernanceQueryCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Key

import           Data.Text (Text)

data GovernanceQueryCmds era
  = GovernanceQueryConstitutionCmd            !(QueryNoArgCmdArgs era)
  | GovernanceQueryGovStateCmd                !(QueryNoArgCmdArgs era)
  | GovernanceQueryDRepStateCmd               !(QueryDRepStateCmdArgs era)
  | GovernanceQueryDRepStakeDistributionCmd   !(QueryDRepStakeDistributionCmdArgs era)
  | GovernanceQueryCommitteeStateCmd          !(QueryNoArgCmdArgs era)
  deriving Show

data QueryNoArgCmdArgs era = QueryNoArgCmdArgs
  { eon                 :: !(ConwayEraOnwards era)
  , nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving Show

data QueryDRepStateCmdArgs era = QueryDRepStateCmdArgs
  { eon                 :: !(ConwayEraOnwards era)
  , nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , drepKeys            :: ![VerificationKeyOrHashOrFile DRepKey]
  , mOutFile            :: !(Maybe (File () Out))
  } deriving Show

data QueryDRepStakeDistributionCmdArgs era = QueryDRepStakeDistributionCmdArgs
  { eon                 :: !(ConwayEraOnwards era)
  , nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , drepKeys            :: ![VerificationKeyOrHashOrFile DRepKey]
  , mOutFile            :: !(Maybe (File () Out))
  } deriving Show

renderGovernanceQueryCmds :: GovernanceQueryCmds era -> Text
renderGovernanceQueryCmds = ("governance query " <>) . \case
  GovernanceQueryConstitutionCmd{}          -> "constitution"
  GovernanceQueryGovStateCmd{}              -> "gov-state"
  GovernanceQueryDRepStateCmd{}             -> "drep-state"
  GovernanceQueryDRepStakeDistributionCmd{} -> "drep-stake-distribution"
  GovernanceQueryCommitteeStateCmd{}        -> "committee-state"
