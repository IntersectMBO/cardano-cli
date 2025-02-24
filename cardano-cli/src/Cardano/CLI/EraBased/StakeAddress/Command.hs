{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.StakeAddress.Command
  ( StakeAddressCmds (..)
  , renderStakeAddressCmds
  )
where

import Cardano.Api.Ledger (Coin)
import Cardano.Api.Shelley

import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Governance
import Cardano.CLI.Type.Key

import Prelude

import Data.Text (Text)

data StakeAddressCmds era
  = StakeAddressKeyGenCmd
      KeyOutputFormat
      (VerificationKeyFile Out)
      (SigningKeyFile Out)
  | StakeAddressKeyHashCmd
      (VerificationKeyOrFile StakeKey)
      (Maybe (File () Out))
  | StakeAddressBuildCmd
      StakeVerifier
      NetworkId
      (Maybe (File () Out))
  | StakeAddressRegistrationCertificateCmd
      (ShelleyBasedEra era)
      StakeIdentifier
      (Maybe Coin)
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
      (Maybe Coin)
      (File () Out)
  | StakeAddressRegistrationAndDelegationCertificateCmd
      (ConwayEraOnwards era)
      StakeIdentifier
      (VerificationKeyOrHashOrFile StakePoolKey)
      Coin
      (File () Out)
  | StakeAddressRegistrationAndVoteDelegationCertificateCmd
      (ConwayEraOnwards era)
      StakeIdentifier
      VoteDelegationTarget
      Coin
      (File () Out)
  | StakeAddressRegistrationStakeAndVoteDelegationCertificateCmd
      (ConwayEraOnwards era)
      StakeIdentifier
      (VerificationKeyOrHashOrFile StakePoolKey)
      VoteDelegationTarget
      Coin
      (File () Out)
  deriving Show

renderStakeAddressCmds :: StakeAddressCmds era -> Text
renderStakeAddressCmds = \case
  StakeAddressBuildCmd{} -> "stake-address build"
  StakeAddressDeregistrationCertificateCmd{} -> "stake-address deregistration-certificate"
  StakeAddressKeyGenCmd{} -> "stake-address key-gen"
  StakeAddressKeyHashCmd{} -> "stake-address key-hash"
  StakeAddressRegistrationCertificateCmd{} -> "stake-address registration-certificate"
  StakeAddressStakeAndVoteDelegationCertificateCmd{} -> "stake-address stake-and-vote-delegation-certificate"
  StakeAddressStakeDelegationCertificateCmd{} -> "stake-address stake-delegation-certificate"
  StakeAddressVoteDelegationCertificateCmd{} -> "stake-address vote-delegation-certificate"
  StakeAddressRegistrationAndDelegationCertificateCmd{} -> "stake-address registration-and-stake-delegation-certificate"
  StakeAddressRegistrationAndVoteDelegationCertificateCmd{} -> "stake-address registration-and-vote-delegation-certificate"
  StakeAddressRegistrationStakeAndVoteDelegationCertificateCmd{} -> "stake-address registration-stake-and-vote-delegation-certificate"
