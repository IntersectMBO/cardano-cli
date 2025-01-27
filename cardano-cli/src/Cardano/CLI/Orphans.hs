{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.CLI.Orphans
  (
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley (VotesMergingConflict, scriptDataToJsonDetailedSchema)

import           Control.Monad.Catch (Exception)
import           Data.Aeson
import           Data.Typeable

instance Exception (FileError ())

instance Exception (FileError TextEnvelopeError)

instance Exception ProtocolParametersConversionError

instance Exception TxBodyError

instance Typeable era => Exception (VotesMergingConflict era)

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
