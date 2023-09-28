{-# LANGUAGE OverloadedStrings #-}

module Test.Cli.MultiAssetParsing
  ( hprop_roundtrip_Value_parse_render
  , hprop_roundtrip_Value_parse_renderPretty
  ) where

import Cardano.Api (parseValue, renderValue, renderValuePretty, valueToList)

import Data.Text qualified as Text
import Text.Parsec qualified as Parsec (parse)

import Test.Gen.Cardano.Api.Typed (genValueDefault)

import Hedgehog (Property, forAll, property, tripping)
import Hedgehog.Gen qualified as Gen

hprop_roundtrip_Value_parse_render :: Property
hprop_roundtrip_Value_parse_render =
  property $ do
    value <- forAll $ Gen.filter (not . null . valueToList) genValueDefault
    tripping
      value
      renderValue
      (Parsec.parse parseValue "" . Text.unpack)

hprop_roundtrip_Value_parse_renderPretty :: Property
hprop_roundtrip_Value_parse_renderPretty =
  property $ do
    value <- forAll $ Gen.filter (not . null . valueToList) genValueDefault
    tripping
      value
      renderValuePretty
      (Parsec.parse parseValue "" . Text.unpack)
