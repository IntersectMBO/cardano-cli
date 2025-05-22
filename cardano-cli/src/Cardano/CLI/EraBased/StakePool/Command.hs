{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.StakePool.Command
  ( StakePoolCmds (..)
  , renderStakePoolCmds
  , StakePoolDeregistrationCertificateCmdArgs (..)
  , StakePoolIdCmdArgs (..)
  , StakePoolMetadataHashCmdArgs (..)
  , StakePoolRegistrationCertificateCmdArgs (..)
  , StakePoolMetadataSource (..)
  )
where

import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger (Coin)
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import Cardano.CLI.EraIndependent.Hash.Command (HashGoal)
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Key

import Prelude

import Data.Text (Text)
import Vary

data StakePoolCmds era
  = StakePoolDeregistrationCertificateCmd !(StakePoolDeregistrationCertificateCmdArgs era)
  | StakePoolIdCmd !(StakePoolIdCmdArgs era)
  | StakePoolMetadataHashCmd !(StakePoolMetadataHashCmdArgs era)
  | StakePoolRegistrationCertificateCmd !(StakePoolRegistrationCertificateCmdArgs era)
  deriving Show

data StakePoolDeregistrationCertificateCmdArgs era
  = StakePoolDeregistrationCertificateCmdArgs
  { sbe :: !(ShelleyBasedEra era)
  , poolVerificationKeyOrFile :: !StakePoolVerificationKeySource
  , retireEpoch :: !EpochNo
  , outFile :: !(File () Out)
  }
  deriving Show

data StakePoolIdCmdArgs era
  = StakePoolIdCmdArgs
  { poolVerificationKeyOrFile :: !StakePoolVerificationKeySource
  , outputFormat :: !(Vary [FormatBech32, FormatHex])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show

data StakePoolMetadataHashCmdArgs era
  = StakePoolMetadataHashCmdArgs
  { poolMetadataSource :: !StakePoolMetadataSource
  , hashGoal :: !(HashGoal (Hash StakePoolMetadata))
  }
  deriving Show

data StakePoolMetadataSource
  = StakePoolMetadataFileIn !(StakePoolMetadataFile In)
  | StakePoolMetadataURL !L.Url
  deriving Show

data StakePoolRegistrationCertificateCmdArgs era
  = StakePoolRegistrationCertificateCmdArgs
  { era :: !(Exp.Era era)
  -- ^ Era in which to register the stake pool.
  , poolVerificationKeyOrFile :: !StakePoolVerificationKeySource
  -- ^ Stake pool verification key.
  , vrfVerificationKeyOrFile :: !(VerificationKeyOrFile VrfKey)
  -- ^ VRF Verification key.
  , poolPledge :: !Coin
  -- ^ Pool pledge.
  , poolCost :: !Coin
  -- ^ Pool cost.
  , poolMargin :: !Rational
  -- ^ Pool margin.
  , rewardStakeVerificationKeyOrFile :: !(VerificationKeyOrFile StakeKey)
  -- ^ Reward account verification staking key.
  , ownerStakeVerificationKeyOrFiles :: ![VerificationKeyOrFile StakeKey]
  -- ^ Pool owner verification staking key(s).
  , relays :: ![StakePoolRelay]
  -- ^ Stake pool relays.
  , mMetadata
      :: !(Maybe (PotentiallyCheckedAnchor StakePoolMetadataReference StakePoolMetadataReference))
  -- ^ Stake pool metadata.
  , network :: !NetworkId
  , outFile :: !(File () Out)
  }
  deriving Show

renderStakePoolCmds :: StakePoolCmds era -> Text
renderStakePoolCmds = \case
  StakePoolDeregistrationCertificateCmd{} ->
    "stake-pool deregistration-certificate"
  StakePoolIdCmd{} ->
    "stake-pool id"
  StakePoolMetadataHashCmd{} ->
    "stake-pool metadata-hash"
  StakePoolRegistrationCertificateCmd{} ->
    "stake-pool registration-certificate"
