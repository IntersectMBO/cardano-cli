module Test.Golden.Shelley.TextEnvelope.Tx.Witness where

import Hedgehog (success)
import Hedgehog.Extras (UnitIO)

{- HLINT ignore "Use camelCase" -}

tasty_golden_shelleyWitness :: UnitIO ()
tasty_golden_shelleyWitness =
  success -- TODO
