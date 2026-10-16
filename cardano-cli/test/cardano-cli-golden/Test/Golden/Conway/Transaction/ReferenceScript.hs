{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Conway.Transaction.ReferenceScript where

import GHC.IO.Exception (ExitCode (ExitFailure))

import Test.Cardano.CLI.Util

import Hedgehog (Property, (===))
import Hedgehog.Extras.Test qualified as H

-- A reference script in a Plutus language the era does not support must fail
-- the command instead of being silently dropped from the output.

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden conway build raw reference script unsupported language/"'@
hprop_golden_conway_build_raw_reference_script_unsupported_language :: Property
hprop_golden_conway_build_raw_reference_script_unsupported_language =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- noteTempFile tempDir "out.json"

    (exitCode, _stdout, stderr) <-
      execDetailCardanoCLI
        [ "conway"
        , "transaction"
        , "build-raw"
        , "--tx-in"
        , "f62cd7bc15d8c6d2c8519fb8d13c57c0157ab6bab50af62bc63706feb966393d#0"
        , "--tx-out"
        , "addr_test1qpmxr8d8jcl25kyz2tz9a9sxv7jxglhddyf475045y8j3zxjcg9vquzkljyfn3rasfwwlkwu7hhm59gzxmsyxf3w9dps8832xh+5000000"
        , "--tx-out-reference-script-file"
        , "test/cardano-cli-golden/files/input/dijkstra/plutus/v4-always-succeeds.plutus"
        , "--fee"
        , "166777"
        , "--out-file"
        , outFile
        ]

    exitCode === ExitFailure 1

    H.diffVsGoldenFileExcludeTrace
      stderr
      "test/cardano-cli-golden/files/golden/conway/transaction/build-raw-reference-script-unsupported-language.out"
