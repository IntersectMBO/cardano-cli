module Test.Golden.Shelley.TextEnvelope.Tx.Witness where

import Test.Cardano.CLI.Util (propertyOnce)

import Hedgehog (Property, success)

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelleyWitness :: Property
hprop_golden_shelleyWitness =
  propertyOnce success
