{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Types.Errors.QueryCmdError
  ( QueryCmdError (..)
  , renderQueryCmdError
  )
where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))
import           Cardano.Api.Consensus as Consensus (EraMismatch (..), PastHorizonException)
import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.Binary (DecoderError)
import           Cardano.CLI.Helpers (HelpersError (..), renderHelpersError)
import           Cardano.CLI.Types.Errors.GenesisCmdError
import           Cardano.CLI.Types.Errors.QueryCmdLocalStateQueryError

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text.Lazy.Builder (toLazyText)
import           Formatting.Buildable (build)

{- HLINT ignore "Move brackets to avoid $" -}
{- HLINT ignore "Redundant flip" -}

data QueryCmdError
  = QueryCmdLocalStateQueryError !QueryCmdLocalStateQueryError
  | QueryCmdConvenienceError !QueryConvenienceError
  | QueryCmdWriteFileError !(FileError ())
  | QueryCmdHelpersError !HelpersError
  | QueryCmdAcquireFailure !AcquiringFailure
  | QueryCmdByronEra
  | QueryCmdEraMismatch !EraMismatch
  | QueryCmdPastHorizon !Consensus.PastHorizonException
  | QueryCmdSystemStartUnavailable
  | QueryCmdGenesisReadError !GenesisCmdError
  | QueryCmdLeaderShipError !LeadershipError
  | QueryCmdTextEnvelopeReadError !(FileError TextEnvelopeError)
  | QueryCmdTextReadError !(FileError InputDecodeError)
  | QueryCmdOpCertCounterReadError !(FileError TextEnvelopeError)
  | QueryCmdProtocolStateDecodeFailure !(LBS.ByteString, DecoderError)
  | QueryCmdPoolStateDecodeError DecoderError
  | QueryCmdStakeSnapshotDecodeError DecoderError
  | QueryCmdUnsupportedNtcVersion !UnsupportedNtcVersionError
  | QueryCmdProtocolParameterConversionError !ProtocolParametersConversionError
  | QueryCmdDRepKeyError !(FileError InputDecodeError)
  | QueryCmdSPOKeyError !(FileError InputDecodeError)
  | QueryCmdCommitteeColdKeyError !(FileError InputDecodeError)
  | QueryCmdCommitteeHotKeyError !(FileError InputDecodeError)
  deriving Show

renderQueryCmdError :: QueryCmdError -> Doc ann
renderQueryCmdError = \case
  QueryCmdLocalStateQueryError lsqErr ->
    prettyError lsqErr
  QueryCmdWriteFileError fileErr ->
    prettyError fileErr
  QueryCmdHelpersError helpersErr ->
    renderHelpersError helpersErr
  QueryCmdAcquireFailure acquireFail ->
    pshow acquireFail
  QueryCmdByronEra ->
    "This query cannot be used for the Byron era"
  QueryCmdEraMismatch (EraMismatch ledgerEra queryEra) ->
    "\nAn error mismatch occurred."
      <> "\nSpecified query era: "
      <> pretty queryEra
      <> "\nCurrent ledger era: "
      <> pretty ledgerEra
  QueryCmdPastHorizon e ->
    "Past horizon: " <> pshow e
  QueryCmdSystemStartUnavailable ->
    "System start unavailable"
  QueryCmdGenesisReadError err' ->
    prettyError err'
  QueryCmdLeaderShipError e ->
    prettyError e
  QueryCmdTextEnvelopeReadError e ->
    prettyError e
  QueryCmdTextReadError e ->
    prettyError e
  QueryCmdOpCertCounterReadError e ->
    prettyError e
  QueryCmdProtocolStateDecodeFailure (_, decErr) ->
    "Failed to decode the protocol state: " <> pretty (toLazyText $ build decErr)
  QueryCmdPoolStateDecodeError decoderError ->
    "Failed to decode PoolState.  Error: " <> pshow decoderError
  QueryCmdStakeSnapshotDecodeError decoderError ->
    "Failed to decode StakeSnapshot.  Error: " <> pshow decoderError
  QueryCmdUnsupportedNtcVersion (UnsupportedNtcVersionError minNtcVersion ntcVersion) ->
    "Unsupported feature for the node-to-client protocol version.\n"
      <> "This query requires at least "
      <> pshow minNtcVersion
      <> " but the node negotiated "
      <> pshow ntcVersion
      <> ".\n"
      <> "Later node versions support later protocol versions (but development protocol versions are not enabled in the node by default)."
  QueryCmdProtocolParameterConversionError ppce ->
    "Failed to convert protocol parameter: " <> prettyError ppce
  QueryCmdConvenienceError qce ->
    pretty $ renderQueryConvenienceError qce
  QueryCmdDRepKeyError e ->
    "Error reading delegation representative key: " <> prettyError e
  QueryCmdSPOKeyError e ->
    "Error reading Stake Pool Operator key: " <> prettyError e
  QueryCmdCommitteeColdKeyError e ->
    "Error reading committee cold key: " <> prettyError e
  QueryCmdCommitteeHotKeyError e ->
    "Error reading committee hot key: " <> prettyError e
