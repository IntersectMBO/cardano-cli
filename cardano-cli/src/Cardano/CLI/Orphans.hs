{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.CLI.Orphans
  (
  )
where

import           Cardano.Api

import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Shelley.LedgerState as L

import           Data.Aeson

-- TODO upstream this orphaned instance to the ledger
instance (L.EraTxOut ledgerera, L.EraGov ledgerera) => ToJSON (L.NewEpochState ledgerera) where
  toJSON (L.NewEpochState nesEL nesBprev nesBCur nesEs nesRu nesPd _stashedAvvm) =
    object
      [ "currentEpoch" .= nesEL
      , "priorBlocks" .= nesBprev
      , "currentEpochBlocks" .= nesBCur
      , "currentEpochState" .= nesEs
      , "rewardUpdate" .= nesRu
      , "currentStakeDistribution" .= nesPd
      ]
