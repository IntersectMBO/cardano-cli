{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.CLI.Orphan
  (
  )
where

import Cardano.Api
import Cardano.Api.Byron qualified as Byron
import Cardano.Api.Experimental as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Type.Error.ScriptDecodeError
import Cardano.Ledger.CertState qualified as L
import Cardano.Ledger.Conway.Governance qualified as L
import Cardano.Ledger.State qualified as L

import Control.Exception
import Data.Aeson
import Data.List qualified as List
import Data.Text (Text)
import Data.Typeable
import Data.Word

instance ToJSON L.DefaultVote where
  toJSON defaultVote =
    case defaultVote of
      L.DefaultNo -> String "DefaultNo"
      L.DefaultAbstain -> String "DefaultAbstain"
      L.DefaultNoConfidence -> String "DefaultNoConfidence"

instance Error [Bech32DecodeError] where
  prettyError errs = vsep $ map prettyError errs

instance Error [RawBytesHexError] where
  prettyError errs = vsep $ map prettyError errs

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

instance Error Byron.GenesisDataError where
  prettyError = pshow

-- TODO: Convert readVerificationKeySource to use CIO. We can then
-- remove this instance
instance
  Error
    ( Either
        ( FileError
            ScriptDecodeError
        )
        (FileError InputDecodeError)
    )
  where
  prettyError = \case
    Left e -> prettyError e
    Right e -> prettyError e

instance Error String where
  prettyError = pretty

instance Error Text where
  prettyError = pretty

instance (Typeable e, Show e, Error e) => Exception (FileError e) where
  displayException = displayError

instance Error [(Word64, TxMetadataRangeError)] where
  prettyError errs =
    mconcat
      [ "Error validating transaction metadata at: " <> "\n"
      , mconcat $
          List.intersperse
            "\n"
            [ "key " <> pshow k <> ":" <> prettyError valErr
            | (k, valErr) <- errs
            ]
      ]

-- Move to cardano-api
instance Convert Era AllegraEraOnwards where
  convert Exp.ConwayEra = AllegraEraOnwardsConway
