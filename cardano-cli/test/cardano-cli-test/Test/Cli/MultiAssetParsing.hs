{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Use camelCase" -}

module Test.Cli.MultiAssetParsing where

import           Cardano.Api (MaryEraOnwards (..), ShelleyBasedEra (..), fromLedgerValue,
                   parseValue, renderValue, renderValuePretty)

import qualified Data.Text as Text
import qualified Text.Parsec as Parsec (parse)

import           Test.Gen.Cardano.Api.Typed (genValueDefault)

import           Hedgehog (Property, forAll, property, tripping)

-- TODO enable these tests after switching completely to ledger types
disable_hprop_roundtrip_Value_parse_render :: Property
disable_hprop_roundtrip_Value_parse_render =
  property $ do
    ledgerValue <- forAll $ genValueDefault MaryEraOnwardsConway
    let value = fromLedgerValue ShelleyBasedEraConway ledgerValue
    tripping
      value
      renderValue
      (Parsec.parse parseValue "" . Text.unpack)

-- TODO enable these tests after switching completely to ledger types
disable_hprop_roundtrip_Value_parse_renderPretty :: Property
disable_hprop_roundtrip_Value_parse_renderPretty =
  property $ do
    ledgerValue <- forAll $ genValueDefault MaryEraOnwardsConway
    let value = fromLedgerValue ShelleyBasedEraConway ledgerValue
    tripping
      value
      renderValuePretty
      (Parsec.parse parseValue "" . Text.unpack)
