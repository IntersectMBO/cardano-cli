{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- | Shelley CLI command types
module Cardano.CLI.Legacy.Commands.StakeAddress
  ( LegacyStakeAddressCmds (..)
  , renderLegacyStakeAddressCmds
  ) where

import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key

import           Prelude

import           Data.Text (Text)

data LegacyStakeAddressCmds
  = StakeAddressKeyGen
      KeyOutputFormat
      (VerificationKeyFile Out)
      (SigningKeyFile Out)
  | StakeAddressKeyHash
      (VerificationKeyOrFile StakeKey)
      (Maybe (File () Out))
  | StakeAddressBuild
      StakeVerifier
      NetworkId
      (Maybe (File () Out))
  | StakeRegistrationCert
      AnyShelleyBasedEra
      StakeIdentifier
      (Maybe Lovelace)
      (File () Out)
  | StakeCredentialDelegationCert
      AnyShelleyBasedEra
      StakeIdentifier
      DelegationTarget
      (File () Out)
  | StakeCredentialDeRegistrationCert
      AnyShelleyBasedEra
      StakeIdentifier
      (Maybe Lovelace)
      (File () Out)
  deriving Show

renderLegacyStakeAddressCmds :: LegacyStakeAddressCmds -> Text
renderLegacyStakeAddressCmds cmd =
  case cmd of
    StakeAddressKeyGen {} -> "stake-address key-gen"
    StakeAddressKeyHash {} -> "stake-address key-hash"
    StakeAddressBuild {} -> "stake-address build"
    StakeRegistrationCert {} -> "stake-address registration-certificate"
    StakeCredentialDelegationCert {} -> "stake-address delegation-certificate"
    StakeCredentialDeRegistrationCert {} -> "stake-address deregistration-certificate"
