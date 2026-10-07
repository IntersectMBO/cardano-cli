{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Address.Info where

import Cardano.Api qualified as Api

import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Text qualified as Text

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog qualified as H

hprop_golden_shelleyAddressInfo :: Property
hprop_golden_shelleyAddressInfo =
  watchdogProp . propertyOnce $ do
    -- Disable as per commit: e69984d797fc3bdd5d71bdd99a0328110d71f6ad
    when False $ do
      let byronBase58 =
            "DdzFFzCqrhsg9F1joqXWJdGKwn6MaNavCqPsrZcjADRjA4ifEtrBmREJZyCojtuexKjMKNFr6CoU7Gx6PPR7pq14JxvPZuuk2xVkzn8p"

      infoText1 <-
        execCardanoCLI
          [ "latest"
          , "address"
          , "info"
          , "--address"
          , byronBase58
          ]

      H.assert $ "Encoding: Base58" `L.isInfixOf` infoText1
      H.assert $ "Era: Byron" `L.isInfixOf` infoText1

      let byronHex =
            "82d818584283581c120e97e4ca7b831373c1060853d4896314e17d567a5723879b9a20eaa101581e581c135a115dd5dba68c28fb7e9409729ffc0503219ff7f9c08e84d13319001a28d0b871"

      infoText2 <-
        execCardanoCLI
          [ "latest"
          , "address"
          , "info"
          , "--address"
          , byronHex
          ]

      H.assert $ "Encoding: Hex" `L.isInfixOf` infoText2
      H.assert $ "Era: Byron" `L.isInfixOf` infoText2

      let shelleyHex = "82065820d8b4a892f2f6f1820d350c207d17d4cd7e7a1f7e0a83059e2d698a65ab8f96ed"

      infoText3 <-
        execCardanoCLI
          [ "latest"
          , "address"
          , "info"
          , "--address"
          , shelleyHex
          ]

      H.assert $ "Encoding: Hex" `L.isInfixOf` infoText3
      H.assert $ "Era: Shelley" `L.isInfixOf` infoText3

-- The CLI must retain the independent fixed header/payload vector through
-- explicit creation and inspection, including its original protected bytes.
hprop_protected_address_creation_and_inspection :: Property
hprop_protected_address_creation_and_inspection = watchdogProp . propertyOnce $ do
  ordinary <-
    H.evalEither $
      Api.deserialiseFromRawBytes
        (Api.AsAddress Api.AsShelleyAddr)
        (BS.cons 0x61 (BS.replicate 28 0x11))
  protectedText <-
    execCardanoCLI
      ["latest", "address", "protect", "--address", Text.unpack $ Api.serialiseAddress ordinary]
  protected <-
    H.evalEither $
      Api.deserialiseFromRawBytes
        (Api.AsAddress Api.AsShelleyAddr)
        (BS.cons 0x69 (BS.replicate 28 0x11))
  protectedText H.=== Text.unpack (Api.serialiseAddress protected)
  info <- execCardanoCLI ["latest", "address", "info", "--address", protectedText]
  H.assert $ "\"protected\": true" `L.isInfixOf` info
  H.assert $ "\"requiresBodySignature\": true" `L.isInfixOf` info
  H.assert $ "\"era\": \"dijkstra\"" `L.isInfixOf` info
  H.assert $ "6911111111111111111111111111111111111111111111111111111111" `L.isInfixOf` info
