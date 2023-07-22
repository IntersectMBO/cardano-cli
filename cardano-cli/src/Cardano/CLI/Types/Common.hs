module Cardano.CLI.Types.Common
  ( TransferDirection(..)
  ) where

-- | Determines the direction in which the MIR certificate will transfer ADA.
data TransferDirection =
    TransferToReserves
  | TransferToTreasury
  deriving Show
