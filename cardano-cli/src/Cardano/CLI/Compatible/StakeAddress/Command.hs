{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Compatible.StakeAddress.Command
  ( CompatibleStakeAddressCmds (..)
  , renderCompatibleStakeAddressCmds
  )
where

import Cardano.Api.Era
import Cardano.Api.IO
import Cardano.Api.Ledger (Coin)

import Cardano.CLI.Type.Key

import Prelude

import Data.Text (Text)

data CompatibleStakeAddressCmds era
  = CompatibleStakeAddressRegistrationCertificateCmd
      (ShelleyBasedEra era)
      StakeIdentifier
      (Maybe (Featured ConwayEraOnwards era Coin))
      -- ^ Deposit required in conway era
      (File () Out)
  | CompatibleStakeAddressStakeDelegationCertificateCmd
      (ShelleyBasedEra era)
      StakeIdentifier
      StakePoolKeyHashSource
      (File () Out)
  deriving Show

renderCompatibleStakeAddressCmds :: CompatibleStakeAddressCmds era -> Text
renderCompatibleStakeAddressCmds =
  (<>) "stake-address " . \case
    CompatibleStakeAddressRegistrationCertificateCmd{} -> "registration-certificate"
    CompatibleStakeAddressStakeDelegationCertificateCmd{} -> "stake-delegation-certificate"
