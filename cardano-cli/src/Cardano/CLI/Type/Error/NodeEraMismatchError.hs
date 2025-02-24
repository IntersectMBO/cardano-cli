{-# LANGUAGE GADTs #-}

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
