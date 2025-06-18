{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.CLI.Type.Error.NodeEraMismatchError
  ( NodeEraMismatchError (..)
  )
where

import Cardano.Api

data NodeEraMismatchError
  = forall era nodeEra.
  NodeEraMismatchError
  { era :: !(CardanoEra era)
  , nodeEra :: !(CardanoEra nodeEra)
  }

deriving instance Show NodeEraMismatchError

instance Error NodeEraMismatchError where
  prettyError (NodeEraMismatchError{era = valueEra, nodeEra = e}) =
    mconcat
      [ "Transactions can only be produced in the same era as the node. Requested era: "
      , pretty valueEra <> ", node era: "
      , pretty e <> "."
      ]
