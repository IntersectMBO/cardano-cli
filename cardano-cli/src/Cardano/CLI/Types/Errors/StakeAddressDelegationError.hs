{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Types.Errors.StakeAddressDelegationError
  ( StakeAddressDelegationError (..)
  ) where

import Cardano.Api

import Data.Text qualified as Text

newtype StakeAddressDelegationError = VoteDelegationNotSupported AnyShelleyToBabbageEra
  deriving (Show)

instance Error StakeAddressDelegationError where
  displayError = \case
    VoteDelegationNotSupported (AnyShelleyToBabbageEra stbe) -> "Vote delegation not supported in " <> eraTxt stbe <> " era."
     where
      eraTxt :: forall era. ShelleyToBabbageEra era -> String
      eraTxt stbe' =
        shelleyToBabbageEraConstraints stbe' $
          Text.unpack . renderEra $
            AnyCardanoEra (cardanoEra @era)
