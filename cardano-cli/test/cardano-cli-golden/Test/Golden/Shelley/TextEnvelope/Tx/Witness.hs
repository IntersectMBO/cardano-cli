module Test.Golden.Shelley.TextEnvelope.Tx.Witness where

import           Hedgehog (Property, property, success)

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelleyWitness :: Property
hprop_golden_shelleyWitness = property success
