module Test.Golden.Byron.Witness where

import Test.Cardano.CLI.Util (propertyOnce, watchdogProp)

import Hedgehog (Property, success)

golden_byronWitness :: Property
golden_byronWitness =
  watchdogProp . propertyOnce $ success -- TODO
