{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Version
  ( hprop_golden_version
  )
where

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog (Property)

{- HLINT ignore "Use camelCase" -}

hprop_golden_version :: Property
hprop_golden_version =
  propertyOnce $ do
    void $
      execCardanoCLI
        [ "version"
        ]
