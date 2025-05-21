module Test.Golden.Byron.TxBody where

import Test.Cardano.CLI.Util (propertyOnce, watchdogProp)

import Hedgehog (Property, success)

{- HLINT ignore "Use camelCase" -}

hprop_golden_byronTxBody :: Property
hprop_golden_byronTxBody =
  watchdogProp . propertyOnce $ success -- TODO
