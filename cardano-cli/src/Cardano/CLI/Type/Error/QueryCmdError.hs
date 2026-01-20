{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Type.Error.QueryCmdError
  ( QueryCmdError (..)
  , renderQueryCmdError
  )
where

import Cardano.Api hiding (QueryInShelleyBasedEra (..))
import Cardano.Api.Consensus as Consensus (PastHorizonException)

import Cardano.CLI.Render
import Cardano.Prelude (SomeException)

import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text.Lazy.Builder (toLazyText)
import Formatting.Buildable (build)

data QueryCmdError
  = QueryCmdWriteFileError !(FileError ())
  | QueryCmdAcquireFailure !AcquiringFailure
  | QueryCmdEraMismatch !EraMismatch
  | QueryCmdNodeToClientDisabled
  | QueryCmdPastHorizon !Consensus.PastHorizonException
  | QueryCmdSystemStartUnavailable
  | QueryCmdLeaderShipError !LeadershipError
  | QueryCmdProtocolStateDecodeFailure !(LBS.ByteString, DecoderError)
  | QueryCmdPoolStateDecodeError DecoderError
  | QueryCmdStakeSnapshotDecodeError DecoderError
  | QueryCmdUnsupportedNtcVersion !UnsupportedNtcVersionError
  | QueryCmdEraNotSupported !AnyCardanoEra
  | QueryBackwardCompatibleError
      !Text
      !SomeException
  deriving Show

instance Error QueryCmdError where
  prettyError = renderQueryCmdError

renderQueryCmdError :: QueryCmdError -> Doc ann
renderQueryCmdError = \case
  -- TODO: This should eventually be removed as
  -- pre-mainnet eras should be handled by the compatible commands
  QueryCmdEraNotSupported anyEra ->
    "This query is not supported in the era: "
      <> pretty anyEra
      <> ".\n"
      <> "Please use a different query or switch to a compatible era."
  QueryCmdWriteFileError fileErr ->
    prettyError fileErr
  QueryCmdAcquireFailure acquireFail ->
    pshow acquireFail
  QueryCmdNodeToClientDisabled -> "Node to client disabled"
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
  QueryCmdLeaderShipError e ->
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
  QueryBackwardCompatibleError cmdText e ->
    renderAnyCmdError cmdText prettyException e
