{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Version
  ( tasty_golden_version
  )
where

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog.Extras (UnitIO)

{- HLINT ignore "Use camelCase" -}

tasty_golden_version :: UnitIO ()
tasty_golden_version = do
  void $
    execCardanoCLI
      [ "version"
      ]
