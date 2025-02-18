{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.GovernanceQueryError where

import Cardano.Api
import Cardano.Api.Consensus (EraMismatch)
import Cardano.Api.Shelley

data GovernanceQueryError
  = GovernanceQueryWriteFileError !(FileError ())
  | GovernanceQueryAcqireFailureError !AcquiringFailure
  | GovernanceQueryUnsupportedNtcVersion !UnsupportedNtcVersionError
  | GovernanceQueryEraMismatch !EraMismatch
  | GovernanceQueryDRepKeyError !(FileError InputDecodeError)
  deriving Show

instance Error GovernanceQueryError where
  prettyError = \case
    GovernanceQueryWriteFileError err ->
      prettyError err
    GovernanceQueryAcqireFailureError err ->
      pshow err
    GovernanceQueryUnsupportedNtcVersion (UnsupportedNtcVersionError minNtcVersion ntcVersion) ->
      vsep
        [ "Unsupported feature for the node-to-client protocol version."
        , "This query requires at least "
            <> pshow minNtcVersion
            <> " but the node negotiated "
            <> pshow ntcVersion
            <> "."
        , "Later node versions support later protocol versions (but development protocol versions are not enabled in the node by default)."
        ]
    GovernanceQueryEraMismatch err ->
      "A query from a certain era was applied to a ledger from a different era: " <> pshow err
    GovernanceQueryDRepKeyError err ->
      "Error reading delegation representative key: " <> prettyError err
