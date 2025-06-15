{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.MultiSig.Address where

import Test.Cardano.CLI.Util as OP

import Hedgehog.Extras (UnitIO)
import Hedgehog.Extras.Test qualified as H

{- HLINT ignore "Use camelCase" -}

tasty_golden_shelleyAllMultiSigAddressBuild :: UnitIO ()
tasty_golden_shelleyAllMultiSigAddressBuild =
  H.moduleWorkspace "tmp" $ \_ -> do
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

tasty_golden_shelleyAnyMultiSigAddressBuild :: UnitIO ()
tasty_golden_shelleyAnyMultiSigAddressBuild =
  H.moduleWorkspace "tmp" $ \_ -> do
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

tasty_golden_shelleyAtLeastMultiSigAddressBuild :: UnitIO ()
tasty_golden_shelleyAtLeastMultiSigAddressBuild =
  H.moduleWorkspace "tmp" $ \_ -> do
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
