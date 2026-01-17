{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Conway.Transaction.BuildRaw where

import Control.Monad (void)
import Data.List (isInfixOf)
import System.Exit (ExitCode (..))

import Test.Cardano.CLI.Util

import Hedgehog
import Hedgehog qualified as H
import Hedgehog.Extras.Test qualified as H

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden conway build raw treasury donation/"'@
hprop_golden_conway_build_raw_treasury_donation :: Property
hprop_golden_conway_build_raw_treasury_donation =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    let goldenFile = "test/cardano-cli-golden/files/golden/conway/build-raw-out.tx"

    -- Key filepaths
    outFile <- noteTempFile tempDir "out.json"

    void $
      execCardanoCLI
        [ "conway"
        , "transaction"
        , "build-raw"
        , "--tx-in"
        , "f62cd7bc15d8c6d2c8519fb8d13c57c0157ab6bab50af62bc63706feb966393d#0"
        , "--tx-out"
        , "addr_test1qpmxr8d8jcl25kyz2tz9a9sxv7jxglhddyf475045y8j3zxjcg9vquzkljyfn3rasfwwlkwu7hhm59gzxmsyxf3w9dps8832xh+1199989833223"
        , "--tx-out"
        , "addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4+10000000"
        , "--current-treasury-value"
        , "543"
        , "--treasury-donation"
        , "1000343"
        , "--fee"
        , "166777"
        , "--out-file"
        , outFile
        ]

    H.diffFileVsGoldenFile outFile goldenFile

-- Current treasury value is not mandatory for donations, see formal spec:
-- \* https://intersectmbo.github.io/formal-ledger-specifications/site/Ledger.Conway.Specification.Utxo.html#sec:the-utxo-transition-system
-- \* https://intersectmbo.github.io/formal-ledger-specifications/site/Notation.html#the-maybe-type
-- And discussion:
-- \* https://discord.com/channels/1136727663583698984/1239888777015590913/1364244737602879498

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden conway build raw donation no current treasury value/"'@
hprop_golden_conway_build_raw_donation_no_current_treasury_value :: Property
hprop_golden_conway_build_raw_donation_no_current_treasury_value =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    let goldenFile = "test/cardano-cli-golden/files/golden/conway/donation-no-current-val.tx"
    -- Key filepaths
    outFile <- noteTempFile tempDir "out.json"

    void $
      execCardanoCLI
        [ "conway"
        , "transaction"
        , "build-raw"
        , "--tx-in"
        , "f62cd7bc15d8c6d2c8519fb8d13c57c0157ab6bab50af62bc63706feb966393d#0"
        , "--tx-out"
        , "addr_test1qpmxr8d8jcl25kyz2tz9a9sxv7jxglhddyf475045y8j3zxjcg9vquzkljyfn3rasfwwlkwu7hhm59gzxmsyxf3w9dps8832xh+1199989833223"
        , "--tx-out"
        , "addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4+10000000"
        , "--treasury-donation"
        , "1000343"
        , "--fee"
        , "166777"
        , "--out-file"
        , outFile
        ]

    H.diffFileVsGoldenFile outFile goldenFile

-- Negative test: Missing --treasury-donation

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden conway build raw donation no treasury donation/"'@
hprop_golden_conway_build_raw_donation_no_treasury_donation :: Property
hprop_golden_conway_build_raw_donation_no_treasury_donation =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- Key filepaths
    outFile <- noteTempFile tempDir "out.json"

    (exitCode, _stdout, stderr) <-
      H.noteShowM $
        execDetailCardanoCLI
          [ "conway"
          , "transaction"
          , "build-raw"
          , "--tx-in"
          , "f62cd7bc15d8c6d2c8519fb8d13c57c0157ab6bab50af62bc63706feb966393d#0"
          , "--tx-out"
          , "addr_test1qpmxr8d8jcl25kyz2tz9a9sxv7jxglhddyf475045y8j3zxjcg9vquzkljyfn3rasfwwlkwu7hhm59gzxmsyxf3w9dps8832xh+1199989833223"
          , "--tx-out"
          , "addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4+10000000"
          , "--current-treasury-value"
          , "1000343"
          , "--fee"
          , "166777"
          , "--out-file"
          , outFile
          ]

    exitCode H.=== ExitFailure 1
    H.assertWith stderr ("Missing: --treasury-donation LOVELACE" `isInfixOf`)
