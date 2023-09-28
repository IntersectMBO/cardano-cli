{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.StakeAddress
  ( StakeAddressCmds (..)
  , renderStakeAddressCmds
  ) where

import Cardano.Api.Shelley

import Cardano.CLI.Types.Common
import Cardano.CLI.Types.Governance
import Cardano.CLI.Types.Key

import Prelude

import Data.Text (Text)

data StakeAddressCmds era
  = StakeAddressKeyGenCmd
      (ShelleyBasedEra era)
      KeyOutputFormat
      (VerificationKeyFile Out)
      (SigningKeyFile Out)
  | StakeAddressKeyHashCmd
      (ShelleyBasedEra era)
      (VerificationKeyOrFile StakeKey)
      (Maybe (File () Out))
  | StakeAddressBuildCmd
      (ShelleyBasedEra era)
      StakeVerifier
      NetworkId
      (Maybe (File () Out))
  | StakeAddressRegistrationCertificateCmd
      (ShelleyBasedEra era)
      StakeIdentifier
      (Maybe Lovelace)
      (File () Out)
  | StakeAddressStakeDelegationCertificateCmd
      (ShelleyBasedEra era)
      StakeIdentifier
      (VerificationKeyOrHashOrFile StakePoolKey)
      (File () Out)
  | StakeAddressStakeAndVoteDelegationCertificateCmd
      (ConwayEraOnwards era)
      StakeIdentifier
      (VerificationKeyOrHashOrFile StakePoolKey)
      VoteDelegationTarget
      (File () Out)
  | StakeAddressVoteDelegationCertificateCmd
      (ConwayEraOnwards era)
      StakeIdentifier
      VoteDelegationTarget
      (File () Out)
  | StakeAddressDeregistrationCertificateCmd
      (ShelleyBasedEra era)
      StakeIdentifier
      (Maybe Lovelace)
      (File () Out)
  deriving (Show)

renderStakeAddressCmds :: StakeAddressCmds era -> Text
renderStakeAddressCmds = \case
  StakeAddressBuildCmd {} -> "stake-address build"
  StakeAddressDeregistrationCertificateCmd {} -> "stake-address deregistration-certificate"
  StakeAddressKeyGenCmd {} -> "stake-address key-gen"
  StakeAddressKeyHashCmd {} -> "stake-address key-hash"
  StakeAddressRegistrationCertificateCmd {} -> "stake-address registration-certificate"
  StakeAddressStakeAndVoteDelegationCertificateCmd {} -> "stake-address stake-and-vote-delegation-certificate"
  StakeAddressStakeDelegationCertificateCmd {} -> "stake-address stake-delegation-certificate"
  StakeAddressVoteDelegationCertificateCmd {} -> "stake-address vote-delegation-certificate"
