{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Types.Errors.StakeAddressDelegationError
  ( StakeAddressDelegationError(..)
  ) where

import           Cardano.Api

newtype StakeAddressDelegationError = VoteDelegationNotSupported (EraInEon ShelleyToBabbageEra) deriving Show

instance Error StakeAddressDelegationError where
  prettyError = \case
    VoteDelegationNotSupported (EraInEon eraInEon) -> "Vote delegation not supported in " <> pretty (toCardanoEra eraInEon) <> " era."
