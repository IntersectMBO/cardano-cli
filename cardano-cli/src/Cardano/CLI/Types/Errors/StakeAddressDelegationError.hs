module Cardano.CLI.Types.Errors.StakeAddressDelegationError
  ( StakeAddressDelegationError(..)
  ) where

import           Cardano.Api

newtype StakeAddressDelegationError =
  VoteDelegationNotSupportedError AnyShelleyToBabbageEra
  deriving Show
