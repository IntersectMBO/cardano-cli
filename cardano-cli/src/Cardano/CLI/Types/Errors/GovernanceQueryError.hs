{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.GovernanceQueryError where

import Cardano.Api
import Cardano.Api.Shelley

import Ouroboros.Consensus.Cardano.Block (EraMismatch)

import Data.Text qualified as T

data GovernanceQueryError
  = GovernanceQueryWriteFileError !(FileError ())
  | GovernanceQueryAcqireFailureError !AcquiringFailure
  | GovernanceQueryEraConsensusModeMismatch !AnyConsensusMode !AnyCardanoEra
  | GovernanceQueryUnsupportedNtcVersion !UnsupportedNtcVersionError
  | GovernanceQueryEraMismatch !EraMismatch
  | GovernanceQueryDRepKeyError !(FileError InputDecodeError)
  deriving (Show)

instance Error GovernanceQueryError where
  displayError = \case
    GovernanceQueryWriteFileError err ->
      displayError err
    GovernanceQueryAcqireFailureError err ->
      show err
    GovernanceQueryEraConsensusModeMismatch mode era ->
      "Era "
        <> T.unpack (renderEra era)
        <> " does not support consensus mode "
        <> T.unpack (renderMode mode)
        <> "."
    GovernanceQueryUnsupportedNtcVersion (UnsupportedNtcVersionError minNtcVersion ntcVersion) ->
      unlines
        [ "Unsupported feature for the node-to-client protocol version."
        , "This query requires at least "
            <> show minNtcVersion
            <> " but the node negotiated "
            <> show ntcVersion
            <> "."
        , "Later node versions support later protocol versions (but development protocol versions are not enabled in the node by default)."
        ]
    GovernanceQueryEraMismatch err ->
      "A query from a certain era was applied to a ledger from a different era: " <> show err
    GovernanceQueryDRepKeyError err ->
      "Error reading delegation representative key: " <> displayError err
