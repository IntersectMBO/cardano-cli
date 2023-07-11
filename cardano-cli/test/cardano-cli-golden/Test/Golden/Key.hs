{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Key
  ( keyTests
  ) where

import qualified Hedgehog as H
import qualified Test.Golden.Key.NonExtendedKey

keyTests :: IO Bool
keyTests =
  H.checkSequential
    $ H.Group "Key command group"
        [ ( "golden_KeyNonExtendedKey_GenesisExtendedVerificationKey"
          , Test.Golden.Key.NonExtendedKey.golden_KeyNonExtendedKey_GenesisExtendedVerificationKey
          )
        , ( "golden_KeyNonExtendedKey_StakeExtendedVerificationKeyShelley"
          , Test.Golden.Key.NonExtendedKey.golden_KeyNonExtendedKey_StakeExtendedVerificationKeyShelley
          )
        ]
