{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.CLI.Orphan
  (
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley (VotesMergingConflict, scriptDataToJsonDetailedSchema)

import Cardano.Ledger.CertState qualified as L
import Cardano.Ledger.Conway.Governance qualified as L
import Cardano.Ledger.State qualified as L

import Data.Aeson

instance ToJSON L.DefaultVote where
  toJSON defaultVote =
    case defaultVote of
      L.DefaultNo -> String "DefaultNo"
      L.DefaultAbstain -> String "DefaultAbstain"
      L.DefaultNoConfidence -> String "DefaultNoConfidence"

instance Error (VotesMergingConflict era) where
  prettyError = pretty . show

-- TODO upstream this orphaned instance to the ledger
instance
  (L.EraTxOut ledgerera, L.EraGov ledgerera, L.EraCertState ledgerera, L.EraStake ledgerera)
  => ToJSON (L.NewEpochState ledgerera)
  where
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
