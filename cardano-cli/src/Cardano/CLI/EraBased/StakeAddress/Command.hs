{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.StakeAddress.Command
  ( StakeAddressCmds (..)
  , renderStakeAddressCmds
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Governance
import Cardano.CLI.Type.Key

import Prelude

import Data.Text (Text)
import Vary (Vary)

data StakeAddressCmds era
  = StakeAddressKeyGenCmd
      (Vary [FormatBech32, FormatTextEnvelope])
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
      (Exp.Era era)
      StakeIdentifier
      (Maybe (Featured ConwayEraOnwards era Coin))
      (File () Out)
  | StakeAddressStakeDelegationCertificateCmd
      (Exp.Era era)
      StakeIdentifier
      StakePoolKeyHashSource
      (File () Out)
  | StakeAddressStakeAndVoteDelegationCertificateCmd
      (ConwayEraOnwards era)
      StakeIdentifier
      StakePoolKeyHashSource
      VoteDelegationTarget
      (File () Out)
  | StakeAddressVoteDelegationCertificateCmd
      (ConwayEraOnwards era)
      StakeIdentifier
      VoteDelegationTarget
      (File () Out)
  | StakeAddressDeregistrationCertificateCmd
      (Exp.Era era)
      StakeIdentifier
      (Maybe (Featured ConwayEraOnwards era Coin))
      (File () Out)
  | StakeAddressRegistrationAndDelegationCertificateCmd
      (ConwayEraOnwards era)
      StakeIdentifier
      StakePoolKeyHashSource
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
      StakePoolKeyHashSource
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
