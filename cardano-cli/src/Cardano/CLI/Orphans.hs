{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.CLI.Orphans
  (
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley (VotesMergingConflict, scriptDataToJsonDetailedSchema)

import Data.Aeson

instance Error (VotesMergingConflict era) where
  prettyError = pretty . show

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

instance ToJSON HashableScriptData where
  toJSON hsd =
    object
      [ "hash" .= hashScriptDataBytes hsd
      , "json" .= scriptDataToJsonDetailedSchema hsd
      ]
