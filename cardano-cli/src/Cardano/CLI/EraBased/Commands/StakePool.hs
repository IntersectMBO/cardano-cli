{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.StakePool
  ( StakePoolCmds (..)
  , renderStakePoolCmds
  , StakePoolDeregistrationCertificateCmdArgs (..)
  , StakePoolIdCmdArgs (..)
  , StakePoolMetadataHashCmdArgs (..)
  , StakePoolRegistrationCertificateCmdArgs (..)
  )
where

import           Cardano.Api.Ledger (Coin)
import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key

import           Prelude

import           Data.Text (Text)

data StakePoolCmds era
  = StakePoolDeregistrationCertificateCmd !(StakePoolDeregistrationCertificateCmdArgs era)
  | StakePoolIdCmd !(StakePoolIdCmdArgs era)
  | StakePoolMetadataHashCmd !(StakePoolMetadataHashCmdArgs era)
  | StakePoolRegistrationCertificateCmd !(StakePoolRegistrationCertificateCmdArgs era)
  deriving Show

data StakePoolDeregistrationCertificateCmdArgs era
  = StakePoolDeregistrationCertificateCmdArgs
  { sbe :: !(ShelleyBasedEra era)
  , poolVerificationKeyOrFile :: !(VerificationKeyOrFile StakePoolKey)
  , retireEpoch :: !EpochNo
  , outFile :: !(File () Out)
  }
  deriving Show

data StakePoolIdCmdArgs era
  = StakePoolIdCmdArgs
  { poolVerificationKeyOrFile :: !(VerificationKeyOrFile StakePoolKey)
  , outputFormat :: !IdOutputFormat
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show

data StakePoolMetadataHashCmdArgs era
  = StakePoolMetadataHashCmdArgs
  { poolMetadataFile :: !(StakePoolMetadataFile In)
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show

data StakePoolRegistrationCertificateCmdArgs era
  = StakePoolRegistrationCertificateCmdArgs
  { sbe :: !(ShelleyBasedEra era)
  -- ^ Era in which to register the stake pool.
  , poolVerificationKeyOrFile :: !(VerificationKeyOrFile StakePoolKey)
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
