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

import Data.Aeson
import Data.Text (Text)
import Text.Parsec qualified as Text

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

instance Error Text.ParseError where
  prettyError = pretty . show

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

instance Error SerialiseAsRawBytesError where
  prettyError = prettyError . unSerialiseAsRawBytesError

instance Error Byron.GenesisDataGenerationError where
  prettyError = pretty . show

instance Error GovernancePollError where
  prettyError = pretty . renderGovernancePollError

instance Error String where
  prettyError = pretty

instance Error Text where
  prettyError = pretty

instance Error (Exp.DeprecatedEra era) where
  prettyError (Exp.DeprecatedEra era) =
    "The era " <> pretty (show era) <> " is deprecated and no longer supported."

instance Error UnsupportedNtcVersionError where
  prettyError (UnsupportedNtcVersionError minNtcVersion ntcVersion) =
    "Unsupported feature for the node-to-client protocol version.\n"
      <> "This query requires at least "
      <> pshow minNtcVersion
      <> " but the node negotiated "
      <> pshow ntcVersion
      <> ".\n"
      <> "Later node versions support later protocol versions (but development protocol versions are not enabled in the node by default)."

instance Error EraMismatch where
  prettyError (EraMismatch ledgerEraName' otherEraName') =
    "The era of the node and the tx do not match. "
      <> "The node is running in the "
      <> pshow ledgerEraName'
      <> " era, but the transaction is for the "
      <> pshow otherEraName'
      <> " era."

instance Error AcquiringFailure where
  prettyError = pshow

instance Error QueryConvenienceError where
  prettyError = pshow . renderQueryConvenienceError
