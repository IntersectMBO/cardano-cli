{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Compatible.StakeAddress.Commands
  ( CompatibleStakeAddressCmds (..)
  , renderCompatibleStakeAddressCmds
  )
where

import Cardano.Api.Ledger (Coin)
import Cardano.Api.Shelley

import Cardano.CLI.Types.Key

import Prelude

import Data.Text (Text)

data CompatibleStakeAddressCmds era
  = CompatibleStakeAddressRegistrationCertificateCmd
      (ShelleyBasedEra era)
      StakeIdentifier
      (Maybe Coin)
      (File () Out)
  | CompatibleStakeAddressStakeDelegationCertificateCmd
      (ShelleyBasedEra era)
      StakeIdentifier
      (VerificationKeyOrHashOrFile StakePoolKey)
      (File () Out)
  deriving Show

renderCompatibleStakeAddressCmds :: CompatibleStakeAddressCmds era -> Text
renderCompatibleStakeAddressCmds =
  (<>) "stake-address " . \case
    CompatibleStakeAddressRegistrationCertificateCmd{} -> "registration-certificate"
    CompatibleStakeAddressStakeDelegationCertificateCmd{} -> "stake-delegation-certificate"
