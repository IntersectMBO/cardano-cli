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
      AnyShelleyBasedEra
      StakeIdentifier
      (Maybe Lovelace)
      (File () Out)
  | StakeAddressDelegationCertificateCmd
      AnyShelleyBasedEra
      StakeIdentifier
      (VerificationKeyOrHashOrFile StakePoolKey)
      (File () Out)
  | StakeAddressDeregistrationCertificateCmd
      AnyShelleyBasedEra
      StakeIdentifier
      (Maybe Lovelace)
      (File () Out)
  deriving Show

renderLegacyStakeAddressCmds :: LegacyStakeAddressCmds -> Text
renderLegacyStakeAddressCmds = \case
  StakeAddressKeyGenCmd {} -> "stake-address key-gen"
  StakeAddressKeyHashCmd {} -> "stake-address key-hash"
  StakeAddressBuildCmd {} -> "stake-address build"
  StakeAddressRegistrationCertificateCmd {} -> "stake-address registration-certificate"
  StakeAddressDelegationCertificateCmd {} -> "stake-address delegation-certificate"
  StakeAddressDeregistrationCertificateCmd {} -> "stake-address deregistration-certificate"
