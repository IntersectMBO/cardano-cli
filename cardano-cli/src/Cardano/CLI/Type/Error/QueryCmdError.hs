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

import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text.Lazy.Builder (toLazyText)
import Formatting.Buildable (build)

{- HLINT ignore "Move brackets to avoid $" -}
{- HLINT ignore "Redundant flip" -}

data QueryCmdError
  = QueryCmdWriteFileError !(FileError ())
  | QueryCmdHelpersError !HelpersError
  | QueryCmdAcquireFailure !AcquiringFailure
  | QueryCmdEraMismatch !EraMismatch
  | QueryCmdPastHorizon !Consensus.PastHorizonException
  | QueryCmdSystemStartUnavailable
  | QueryCmdGenesisReadError !GenesisCmdError
  | QueryCmdLeaderShipError !LeadershipError
  | QueryCmdProtocolStateDecodeFailure !(LBS.ByteString, DecoderError)
  | QueryCmdPoolStateDecodeError DecoderError
  | QueryCmdStakeSnapshotDecodeError DecoderError
  | QueryCmdUnsupportedNtcVersion !UnsupportedNtcVersionError
  | QueryCmdEraNotSupported !AnyCardanoEra
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
  -- TODO: This should eventually be removed as
  -- pre-mainnet eras should be handled by the compatible commands
  QueryCmdEraNotSupported anyEra ->
    "This query is not supported in the era: "
      <> pretty anyEra
      <> ".\n"
      <> "Please use a different query or switch to a compatible era."
  QueryCmdWriteFileError fileErr ->
    prettyError fileErr
  QueryCmdHelpersError helpersErr ->
    renderHelpersError helpersErr
  QueryCmdAcquireFailure acquireFail ->
    pshow acquireFail
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
