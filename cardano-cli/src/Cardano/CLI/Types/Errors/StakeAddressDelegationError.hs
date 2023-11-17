{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Types.Errors.StakeAddressDelegationError
  ( StakeAddressDelegationError(..)
  ) where

import           Cardano.Api
import           Cardano.Api.Pretty

import qualified Data.Text as Text

newtype StakeAddressDelegationError = VoteDelegationNotSupported (EraInEon ShelleyToBabbageEra) deriving Show

instance Error StakeAddressDelegationError where
  prettyError = \case
    VoteDelegationNotSupported (EraInEon eraInEon) -> "Vote delegation not supported in " <> pshow eraTxt <> " era."
      where
        cEra = toCardanoEra eraInEon
        eraTxt = cardanoEraConstraints cEra $ Text.unpack . renderEra $ AnyCardanoEra cEra
