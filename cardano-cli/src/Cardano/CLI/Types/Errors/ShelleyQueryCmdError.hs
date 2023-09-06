{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Types.Errors.ShelleyQueryCmdError
  ( ShelleyQueryCmdError(..)
  , renderShelleyQueryCmdError
  ) where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))
import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.Binary (DecoderError)
import           Cardano.CLI.Helpers (HelpersError (..), renderHelpersError)
import           Cardano.CLI.Types.Errors.ShelleyGenesisCmdError
import           Cardano.CLI.Types.Errors.ShelleyQueryCmdLocalStateQueryError
import           Ouroboros.Consensus.Cardano.Block as Consensus (EraMismatch (..))
import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (toLazyText)
import           Formatting.Buildable (build)

{- HLINT ignore "Move brackets to avoid $" -}
{- HLINT ignore "Redundant flip" -}

data ShelleyQueryCmdError
  = ShelleyQueryCmdLocalStateQueryError !ShelleyQueryCmdLocalStateQueryError
  | ShelleyQueryCmdConvenienceError !QueryConvenienceError
  | ShelleyQueryCmdWriteFileError !(FileError ())
  | ShelleyQueryCmdHelpersError !HelpersError
  | ShelleyQueryCmdAcquireFailure !AcquiringFailure
  | ShelleyQueryCmdEraConsensusModeMismatch !AnyConsensusMode !AnyCardanoEra
  | ShelleyQueryCmdByronEra
  | ShelleyQueryCmdEraMismatch !EraMismatch
  | ShelleyQueryCmdUnsupportedMode !AnyConsensusMode
  | ShelleyQueryCmdPastHorizon !Qry.PastHorizonException
  | ShelleyQueryCmdSystemStartUnavailable
  | ShelleyQueryCmdGenesisReadError !ShelleyGenesisCmdError
  | ShelleyQueryCmdLeaderShipError !LeadershipError
  | ShelleyQueryCmdTextEnvelopeReadError !(FileError TextEnvelopeError)
  | ShelleyQueryCmdTextReadError !(FileError InputDecodeError)
  | ShelleyQueryCmdOpCertCounterReadError !(FileError TextEnvelopeError)
  | ShelleyQueryCmdProtocolStateDecodeFailure !(LBS.ByteString, DecoderError)
  | ShelleyQueryCmdPoolStateDecodeError DecoderError
  | ShelleyQueryCmdStakeSnapshotDecodeError DecoderError
  | ShelleyQueryCmdUnsupportedNtcVersion !UnsupportedNtcVersionError
  | ShelleyQueryCmdProtocolParameterConversionError !ProtocolParametersConversionError
  deriving Show

renderShelleyQueryCmdError :: ShelleyQueryCmdError -> Text
renderShelleyQueryCmdError err =
  case err of
    ShelleyQueryCmdLocalStateQueryError lsqErr -> renderLocalStateQueryError lsqErr
    ShelleyQueryCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyQueryCmdHelpersError helpersErr -> renderHelpersError helpersErr
    ShelleyQueryCmdAcquireFailure acquireFail -> Text.pack $ show acquireFail
    ShelleyQueryCmdByronEra -> "This query cannot be used for the Byron era"
    ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) (AnyCardanoEra era) ->
      "Consensus mode and era mismatch. Consensus mode: " <> textShow cMode <>
      " Era: " <> textShow era
    ShelleyQueryCmdEraMismatch (EraMismatch ledgerEra queryEra) ->
      "\nAn error mismatch occurred." <> "\nSpecified query era: " <> queryEra <>
      "\nCurrent ledger era: " <> ledgerEra
    ShelleyQueryCmdUnsupportedMode mode -> "Unsupported mode: " <> renderMode mode
    ShelleyQueryCmdPastHorizon e -> "Past horizon: " <> textShow e
    ShelleyQueryCmdSystemStartUnavailable -> "System start unavailable"
    ShelleyQueryCmdGenesisReadError err' -> Text.pack $ displayError err'
    ShelleyQueryCmdLeaderShipError e -> Text.pack $ displayError e
    ShelleyQueryCmdTextEnvelopeReadError e -> Text.pack $ displayError e
    ShelleyQueryCmdTextReadError e -> Text.pack $ displayError e
    ShelleyQueryCmdOpCertCounterReadError e -> Text.pack $ displayError e
    ShelleyQueryCmdProtocolStateDecodeFailure (_, decErr) ->
      "Failed to decode the protocol state: " <> toStrict (toLazyText $ build decErr)
    ShelleyQueryCmdPoolStateDecodeError decoderError ->
      "Failed to decode PoolState.  Error: " <> Text.pack (show decoderError)
    ShelleyQueryCmdStakeSnapshotDecodeError decoderError ->
      "Failed to decode StakeSnapshot.  Error: " <> Text.pack (show decoderError)
    ShelleyQueryCmdUnsupportedNtcVersion (UnsupportedNtcVersionError minNtcVersion ntcVersion) ->
      "Unsupported feature for the node-to-client protocol version.\n" <>
      "This query requires at least " <> textShow minNtcVersion <> " but the node negotiated " <> textShow ntcVersion <> ".\n" <>
      "Later node versions support later protocol versions (but development protocol versions are not enabled in the node by default)."
    ShelleyQueryCmdProtocolParameterConversionError ppce ->
      Text.pack $ "Failed to convert protocol parameter: " <> displayError ppce
    ShelleyQueryCmdConvenienceError qce -> renderQueryConvenienceError qce
