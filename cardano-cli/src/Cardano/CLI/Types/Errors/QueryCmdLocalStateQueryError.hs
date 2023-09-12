module Cardano.CLI.Types.Errors.QueryCmdLocalStateQueryError
  ( QueryCmdLocalStateQueryError(..)
  , renderLocalStateQueryError
  ) where

import           Cardano.Api

import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..))

import           Data.Text (Text)

-- | An error that can occur while querying a node's local state.
newtype QueryCmdLocalStateQueryError
  = EraMismatchError EraMismatch
  -- ^ A query from a certain era was applied to a ledger from a different
  -- era.
  deriving (Eq, Show)

renderLocalStateQueryError :: QueryCmdLocalStateQueryError -> Text
renderLocalStateQueryError lsqErr =
  case lsqErr of
    EraMismatchError err ->
      "A query from a certain era was applied to a ledger from a different era: " <> textShow err
