{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Compatible.StakePool.Commands
  ( CompatibleStakePoolCmds (..)
  , renderCompatibleStakePoolCmds
  , CompatibleStakePoolRegistrationCertificateCmdArgs (..)
  )
where

import Cardano.Api.Ledger (Coin)
import Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import Cardano.CLI.Types.Common
import Cardano.CLI.Types.Key

import Prelude

import Data.Text (Text)

data CompatibleStakePoolCmds era
  = CompatibleStakePoolRegistrationCertificateCmd
      !(CompatibleStakePoolRegistrationCertificateCmdArgs era)
  deriving Show

data CompatibleStakePoolRegistrationCertificateCmdArgs era
  = CompatibleStakePoolRegistrationCertificateCmdArgs
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

renderCompatibleStakePoolCmds :: CompatibleStakePoolCmds era -> Text
renderCompatibleStakePoolCmds =
  (<>) "stake-pool " . \case
    CompatibleStakePoolRegistrationCertificateCmd{} ->
      "registration-certificate"
