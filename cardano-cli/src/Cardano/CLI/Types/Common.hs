{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.CLI.Types.Common
  ( AnyShelleyToBabbageEra(..)
  , TransferDirection(..)
  ) where

import           Cardano.Api

-- | Determines the direction in which the MIR certificate will transfer ADA.
data TransferDirection =
    TransferToReserves
  | TransferToTreasury
  deriving Show

-- TODO Use the one from cardano-api
data AnyShelleyToBabbageEra where
  AnyShelleyToBabbageEra :: ShelleyToBabbageEra era -> AnyShelleyToBabbageEra

deriving instance Show AnyShelleyToBabbageEra
