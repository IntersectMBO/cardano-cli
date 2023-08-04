{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.Types.Common
  ( Constitution(..)
  , OpCertCounter
  , TransferDirection(..)
  ) where

import           Cardano.Api

import           Data.Text (Text)

-- | Determines the direction in which the MIR certificate will transfer ADA.
data TransferDirection =
    TransferToReserves
  | TransferToTreasury
  deriving Show

data OpCertCounter

data Constitution
  = ConstitutionFromFile (File () In)
  | ConstitutionFromText Text deriving Show
