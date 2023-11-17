{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.StakeAddressRegistrationError
  ( StakeAddressRegistrationError(..)
  ) where

import           Cardano.Api

data StakeAddressRegistrationError = StakeAddressRegistrationDepositRequired
  deriving Show

instance Error StakeAddressRegistrationError where
  prettyError = \case
    StakeAddressRegistrationDepositRequired -> "Stake address deposit required."
