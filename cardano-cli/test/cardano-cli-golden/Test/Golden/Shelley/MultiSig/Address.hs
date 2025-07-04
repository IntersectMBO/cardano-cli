{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.MultiSig.Address where

import Test.Cardano.CLI.Util as OP

import Hedgehog (Property)
import Hedgehog.Extras.Test qualified as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelleyAllMultiSigAddressBuild :: Property
hprop_golden_shelleyAllMultiSigAddressBuild =
  watchdogProp . propertyOnce $ do
    allMultiSigFp <- noteInputFile "test/cardano-cli-golden/files/input/shelley/multisig/scripts/all"

    allMultiSigAddress <-
      execCardanoCLI
        [ "latest"
        , "address"
        , "build"
        , "--payment-script-file"
        , allMultiSigFp
        , "--mainnet"
        ]

    goldenAllMultiSigAddrFp <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/multisig/addresses/all"

    goldenAllMs <- H.readFile goldenAllMultiSigAddrFp

    equivalence allMultiSigAddress goldenAllMs

hprop_golden_shelleyAnyMultiSigAddressBuild :: Property
hprop_golden_shelleyAnyMultiSigAddressBuild =
  watchdogProp . propertyOnce $ do
    anyMultiSigFp <- noteInputFile "test/cardano-cli-golden/files/input/shelley/multisig/scripts/any"

    anyMultiSigAddress <-
      execCardanoCLI
        [ "latest"
        , "address"
        , "build"
        , "--payment-script-file"
        , anyMultiSigFp
        , "--mainnet"
        ]

    goldenAnyMultiSigAddrFp <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/multisig/addresses/any"

    goldenAnyMs <- H.readFile goldenAnyMultiSigAddrFp

    equivalence anyMultiSigAddress goldenAnyMs

hprop_golden_shelleyAtLeastMultiSigAddressBuild :: Property
hprop_golden_shelleyAtLeastMultiSigAddressBuild =
  watchdogProp . propertyOnce $ do
    atLeastMultiSigFp <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/multisig/scripts/atleast"

    atLeastMultiSigAddress <-
      execCardanoCLI
        [ "latest"
        , "address"
        , "build"
        , "--payment-script-file"
        , atLeastMultiSigFp
        , "--mainnet"
        ]

    goldenAtLeastMultiSigAddrFp <-
      H.note "test/cardano-cli-golden/files/golden/shelley/multisig/addresses/atleast"

    H.diffVsGoldenFile atLeastMultiSigAddress goldenAtLeastMultiSigAddrFp
