module Test.Golden.Byron.TxBody where

import Hedgehog (success)
import Hedgehog.Extras (UnitIO)

{- HLINT ignore "Use camelCase" -}

tasty_golden_byronTxBody :: UnitIO ()
tasty_golden_byronTxBody =
  success -- TODO
