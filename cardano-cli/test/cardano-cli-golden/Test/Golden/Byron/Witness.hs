module Test.Golden.Byron.Witness where

import Hedgehog (success)
import Hedgehog.Extras (UnitIO)

{- HLINT ignore "Use camelCase" -}

golden_byronWitness :: UnitIO ()
golden_byronWitness =
  success -- TODO
