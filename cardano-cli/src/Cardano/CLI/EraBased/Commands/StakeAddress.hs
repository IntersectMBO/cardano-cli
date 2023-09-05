{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.StakeAddress
  ( StakeAddressCmds (..)
  , renderStakeAddressCmds
  ) where

import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key

import           Prelude

import           Data.Text (Text)

data StakeAddressCmds era
  = StakeAddressKeyGen
      (ShelleyBasedEra era)
      KeyOutputFormat
      (VerificationKeyFile Out)
      (SigningKeyFile Out)
  | StakeAddressKeyHash
      (ShelleyBasedEra era)
      (VerificationKeyOrFile StakeKey)
      (Maybe (File () Out))
  | StakeAddressBuild
      (ShelleyBasedEra era)
      StakeVerifier
      NetworkId
      (Maybe (File () Out))
  | StakeRegistrationCert
      (ShelleyBasedEra era)
      StakeIdentifier
      (Maybe Lovelace)
      (File () Out)
  | StakeCredentialDelegationCert
      (ShelleyBasedEra era)
      StakeIdentifier
      DelegationTarget
      (File () Out)
  | StakeCredentialDeRegistrationCert
      (ShelleyBasedEra era)
      StakeIdentifier
      (Maybe Lovelace)
      (File () Out)
  deriving Show

renderStakeAddressCmds :: StakeAddressCmds era -> Text
renderStakeAddressCmds = \case
  StakeAddressKeyGen {} -> "stake-address key-gen"
  StakeAddressKeyHash {} -> "stake-address key-hash"
  StakeAddressBuild {} -> "stake-address build"
  StakeRegistrationCert {} -> "stake-address registration-certificate"
  StakeCredentialDelegationCert {} -> "stake-address delegation-certificate"
  StakeCredentialDeRegistrationCert {} -> "stake-address deregistration-certificate"
