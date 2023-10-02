{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Types.Errors.StakeAddressDelegationError
  ( StakeAddressDelegationError(..)
  ) where

import           Cardano.Api

import qualified Data.Text as Text

newtype StakeAddressDelegationError = VoteDelegationNotSupported (AnyEraInEon ShelleyToBabbageEra) deriving Show

instance Error StakeAddressDelegationError where
  displayError = \case
    VoteDelegationNotSupported (AnyEraInEon eraInEon) -> "Vote delegation not supported in " <> eraTxt <> " era."
      where
        cEra = toCardanoEra eraInEon
        eraTxt = cardanoEraConstraints cEra $ Text.unpack . renderEra $ AnyCardanoEra cEra
