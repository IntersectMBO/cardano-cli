{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Type.Error.QueryCmdError
  ( QueryCmdError (..)
  , renderQueryCmdError
  , mkEraMismatchError
  )
where

import Cardano.Api hiding (QueryInShelleyBasedEra (..))
import Cardano.Api.Consensus as Consensus (PastHorizonException)

import Cardano.Binary (DecoderError)
import Cardano.CLI.Helper (HelpersError (..), renderHelpersError)
import Cardano.CLI.Type.Error.GenesisCmdError
import Cardano.CLI.Type.Error.NodeEraMismatchError (NodeEraMismatchError (..))

import Control.Exception (SomeException)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text (Text)
import Data.Text.Lazy.Builder (toLazyText)
import Formatting.Buildable (build)

{- HLINT ignore "Move brackets to avoid $" -}
{- HLINT ignore "Redundant flip" -}

data QueryCmdError
  = QueryCmdConvenienceError !QueryConvenienceError
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
  | QueryBackwardCompatibleError
      Text
      -- ^ Command that was run
      SomeException
      -- ^ An exception that was thrown
  deriving Show

instance Error QueryCmdError where
  prettyError = renderQueryCmdError

mkEraMismatchError :: NodeEraMismatchError -> QueryCmdError
mkEraMismatchError NodeEraMismatchError{nodeEra, era} =
  QueryCmdEraMismatch $
    EraMismatch
      { ledgerEraName = docToText $ pretty nodeEra
      , otherEraName = docToText $ pretty era
      }

renderQueryCmdError :: QueryCmdError -> Doc ann
renderQueryCmdError = \case
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
  QueryBackwardCompatibleError cmdText err ->
    "Backward compatible error for command: "
      <> pretty cmdText
      <> "\n"
      <> prettyException err
