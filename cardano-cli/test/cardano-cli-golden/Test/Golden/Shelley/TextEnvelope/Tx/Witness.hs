module Test.Golden.Shelley.TextEnvelope.Tx.Witness where

import Test.Cardano.CLI.Util (propertyOnce)

import Hedgehog (Property, success)

hprop_golden_shelleyWitness :: Property
hprop_golden_shelleyWitness =
  propertyOnce success
