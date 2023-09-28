{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.StakePool
  ( StakePoolCmds (..)
  , renderStakePoolCmds
  ) where

import Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import Cardano.CLI.Types.Common
import Cardano.CLI.Types.Key

import Prelude

import Data.Text (Text)

data StakePoolCmds era
  = StakePoolDeregistrationCertificateCmd
      (ShelleyBasedEra era)
      -- ^ Era in which to retire the stake pool.
      (VerificationKeyOrFile StakePoolKey)
      -- ^ Stake pool verification key.
      EpochNo
      -- ^ Epoch in which to retire the stake pool.
      (File () Out)
  | StakePoolIdCmd
      (VerificationKeyOrFile StakePoolKey)
      IdOutputFormat
      (Maybe (File () Out))
  | StakePoolMetadataHashCmd
      (StakePoolMetadataFile In)
      (Maybe (File () Out))
  | StakePoolRegistrationCertificateCmd
      (ShelleyBasedEra era)
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
  deriving (Show)

renderStakePoolCmds :: StakePoolCmds era -> Text
renderStakePoolCmds = \case
  StakePoolDeregistrationCertificateCmd {} ->
    "stake-pool deregistration-certificate"
  StakePoolIdCmd {} ->
    "stake-pool id"
  StakePoolMetadataHashCmd {} ->
    "stake-pool metadata-hash"
  StakePoolRegistrationCertificateCmd {} ->
    "stake-pool registration-certificate"
