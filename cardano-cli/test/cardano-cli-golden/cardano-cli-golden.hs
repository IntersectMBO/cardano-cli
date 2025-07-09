{-# LANGUAGE FlexibleInstances #-}

module Main (main, ingredients, tests) where

import Prelude

import Data.List qualified as L
import Data.String (fromString)
import System.Environment qualified as E

import Test.Golden.Byron.SigningKeys qualified
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as H
import Test.Tasty.Ingredients qualified as T

{- HLINT ignore "Use let" -}

tests :: IO T.TestTree
tests = do
  t1 <-
    pure $
      H.testPropertyNamed
        "deserialise nonLegacy signing Key"
        (fromString "Test.Golden.Byron.SigningKeys.hprop_deserialise_nonLegacy_signing_Key")
        Test.Golden.Byron.SigningKeys.hprop_deserialise_nonLegacy_signing_Key

  pure $
    T.testGroup
      "test/cardano-cli-golden/cardano-cli-golden.hs"
      (L.replicate 170 t1)

ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

main :: IO ()
main = do
  args <- E.getArgs
  E.withArgs ([] <> args) $ tests >>= T.defaultMainWithIngredients ingredients
