module Test.Golden.Byron.Witness where

import Hedgehog (Property, property, success)

{- HLINT ignore "Use camelCase" -}

golden_byronWitness :: Property
golden_byronWitness = property success -- TODO
