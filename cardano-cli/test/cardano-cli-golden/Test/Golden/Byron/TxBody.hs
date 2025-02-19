module Test.Golden.Byron.TxBody where

import Hedgehog (Property, property, success)

{- HLINT ignore "Use camelCase" -}

hprop_golden_byronTxBody :: Property
hprop_golden_byronTxBody = property success -- TODO
