{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.QueryCmdLocalStateQueryError
  ( QueryCmdLocalStateQueryError(..)
  , renderLocalStateQueryError
  ) where

import           Cardano.Api.Pretty

import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..))

-- | An error that can occur while querying a node's local state.
newtype QueryCmdLocalStateQueryError
  = EraMismatchError EraMismatch
  -- ^ A query from a certain era was applied to a ledger from a different
  -- era.
  deriving (Eq, Show)

renderLocalStateQueryError :: QueryCmdLocalStateQueryError -> Doc ann
renderLocalStateQueryError = \case
  EraMismatchError err ->
    "A query from a certain era was applied to a ledger from a different era: " <> pshow err
