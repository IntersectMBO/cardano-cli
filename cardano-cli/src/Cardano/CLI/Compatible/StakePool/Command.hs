{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Compatible.StakePool.Command
  ( CompatibleStakePoolCmds (..)
  , renderCompatibleStakePoolCmds
  , CompatibleStakePoolRegistrationCertificateCmdArgs (..)
  )
where

import Cardano.Api

import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Key

import Prelude

newtype CompatibleStakePoolCmds era
  = CompatibleStakePoolRegistrationCertificateCmd
      (CompatibleStakePoolRegistrationCertificateCmdArgs era)
  deriving Show

data CompatibleStakePoolRegistrationCertificateCmdArgs era
  = CompatibleStakePoolRegistrationCertificateCmdArgs
  { sbe :: !(ShelleyBasedEra era)
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

renderCompatibleStakePoolCmds :: CompatibleStakePoolCmds era -> Text
renderCompatibleStakePoolCmds =
  (<>) "stake-pool " . \case
    CompatibleStakePoolRegistrationCertificateCmd{} ->
      "registration-certificate"
