{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.CLI.Orphans
  (
  )
where

import           Cardano.Api
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley (scriptDataToJsonDetailedSchema)

import           Data.Aeson
import           Data.Default ()

-- FIXME remove Data.Default () import. This is only to force usage of data-default < 0.8 and to work around unused pacakge warning.

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

-- TODO Delete me when added in API
instance Convert Exp.Era MaryEraOnwards where
  convert = \case
    Exp.BabbageEra -> MaryEraOnwardsBabbage
    Exp.ConwayEra -> MaryEraOnwardsConway
