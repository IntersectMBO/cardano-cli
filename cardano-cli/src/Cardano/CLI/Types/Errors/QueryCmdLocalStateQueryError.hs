{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.CLI.Types.Errors.QueryCmdLocalStateQueryError
  ( QueryCmdLocalStateQueryError(..)
  , mkEraMismatchError
  , renderLocalStateQueryError
  ) where

import           Cardano.Api.Pretty

import           Cardano.CLI.Types.Errors.NodeEraMismatchError
import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..))

-- | An error that can occur while querying a node's local state.
newtype QueryCmdLocalStateQueryError
  = EraMismatchError EraMismatch
  -- ^ A query from a certain era was applied to a ledger from a different
  -- era.
  deriving (Eq, Show)

mkEraMismatchError :: NodeEraMismatchError -> QueryCmdLocalStateQueryError
mkEraMismatchError NodeEraMismatchError{nodeEra, era} =
  EraMismatchError EraMismatch{ ledgerEraName = docToText $ pretty nodeEra
                              , otherEraName = docToText $ pretty era}

renderLocalStateQueryError :: QueryCmdLocalStateQueryError -> Doc ann
renderLocalStateQueryError = \case
  EraMismatchError err ->
    "A query from a certain era was applied to a ledger from a different era: " <> pshow err
