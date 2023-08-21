{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Commands.Pool
  ( LegacyPoolCmds (..)
  , renderLegacyPoolCmds
  ) where

import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key

import           Prelude

import           Data.Text (Text)

data LegacyPoolCmds
  = PoolRegistrationCert
      AnyShelleyBasedEra
      -- ^ Era in which to register the stake pool.
      (VerificationKeyOrFile StakePoolKey)
      -- ^ Stake pool verification key.
      (VerificationKeyOrFile VrfKey)
      -- ^ VRF Verification key.
      Lovelace
      -- ^ Pool pledge.
      Lovelace
      -- ^ Pool cost.
      Rational
      -- ^ Pool margin.
      (VerificationKeyOrFile StakeKey)
      -- ^ Reward account verification staking key.
      [VerificationKeyOrFile StakeKey]
      -- ^ Pool owner verification staking key(s).
      [StakePoolRelay]
      -- ^ Stake pool relays.
      (Maybe StakePoolMetadataReference)
      -- ^ Stake pool metadata.
      NetworkId
      (File () Out)
  | PoolRetirementCert
      AnyShelleyBasedEra
      -- ^ Era in which to retire the stake pool.
      (VerificationKeyOrFile StakePoolKey)
      -- ^ Stake pool verification key.
      EpochNo
      -- ^ Epoch in which to retire the stake pool.
      (File () Out)
  | PoolGetId
      (VerificationKeyOrFile StakePoolKey)
      IdOutputFormat
      (Maybe (File () Out))
  | PoolMetadataHash (StakePoolMetadataFile In) (Maybe (File () Out))
  deriving Show

renderLegacyPoolCmds :: LegacyPoolCmds -> Text
renderLegacyPoolCmds = \case
  PoolRegistrationCert {} -> "stake-pool registration-certificate"
  PoolRetirementCert {} -> "stake-pool deregistration-certificate"
  PoolGetId {} -> "stake-pool id"
  PoolMetadataHash {} -> "stake-pool metadata-hash"
