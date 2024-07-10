{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Conway.Transaction.BuildRaw where

import           Control.Monad (void)

import           Test.Cardano.CLI.Util

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Golden as H

import           Data.List (isInfixOf)
import           System.Exit (ExitCode (..))

{- HLINT ignore "Use camelCase" -}

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden conway build raw treasury donation/"'@
hprop_golden_conway_build_raw_treasury_donation :: Property
hprop_golden_conway_build_raw_treasury_donation = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  let goldenFile = "test/cardano-cli-golden/files/golden/conway/build-raw-out.tx"

  -- Key filepaths
  outFile <- noteTempFile tempDir "out.json"

  void $ execCardanoCLI
    [ "conway", "transaction", "build-raw"
    , "--tx-in", "f62cd7bc15d8c6d2c8519fb8d13c57c0157ab6bab50af62bc63706feb966393d#0"
    , "--tx-out", "addr_test1qpmxr8d8jcl25kyz2tz9a9sxv7jxglhddyf475045y8j3zxjcg9vquzkljyfn3rasfwwlkwu7hhm59gzxmsyxf3w9dps8832xh+1199989833223"
    , "--tx-out", "addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4+10000000"
    , "--current-treasury-value", "543"
    , "--treasury-donation", "1000343"
    , "--fee", "166777"
    , "--out-file", outFile
    ]

  H.diffFileVsGoldenFile outFile goldenFile

-- Negative test: Missing --current-treasury-value
-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden conway build raw donation no current treasury value/"'@
hprop_golden_conway_build_raw_donation_no_current_treasury_value :: Property
hprop_golden_conway_build_raw_donation_no_current_treasury_value  = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do

  -- Key filepaths
  outFile <- noteTempFile tempDir "out.json"

  (exitCode, _stdout, stderr) <- H.noteShowM $ execDetailCardanoCLI
    [ "conway", "transaction", "build-raw"
    , "--tx-in", "f62cd7bc15d8c6d2c8519fb8d13c57c0157ab6bab50af62bc63706feb966393d#0"
    , "--tx-out", "addr_test1qpmxr8d8jcl25kyz2tz9a9sxv7jxglhddyf475045y8j3zxjcg9vquzkljyfn3rasfwwlkwu7hhm59gzxmsyxf3w9dps8832xh+1199989833223"
    , "--tx-out", "addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4+10000000"
    , "--treasury-donation", "1000343"
    , "--fee", "166777"
    , "--out-file", outFile
    ]

  exitCode H.=== ExitFailure 1
  H.assertWith stderr ("Missing: --current-treasury-value LOVELACE" `isInfixOf`)

-- Negative test: Missing --treasury-donation
-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden conway build raw donation no treasury donation/"'@
hprop_golden_conway_build_raw_donation_no_treasury_donation :: Property
hprop_golden_conway_build_raw_donation_no_treasury_donation  = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do

  -- Key filepaths
  outFile <- noteTempFile tempDir "out.json"

  (exitCode, _stdout, stderr) <- H.noteShowM $ execDetailCardanoCLI
    [ "conway", "transaction", "build-raw"
    , "--tx-in", "f62cd7bc15d8c6d2c8519fb8d13c57c0157ab6bab50af62bc63706feb966393d#0"
    , "--tx-out", "addr_test1qpmxr8d8jcl25kyz2tz9a9sxv7jxglhddyf475045y8j3zxjcg9vquzkljyfn3rasfwwlkwu7hhm59gzxmsyxf3w9dps8832xh+1199989833223"
    , "--tx-out", "addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4+10000000"
    , "--current-treasury-value", "1000343"
    , "--fee", "166777"
    , "--out-file", outFile
    ]

  exitCode H.=== ExitFailure 1
  H.assertWith stderr ("Missing: --treasury-donation LOVELACE" `isInfixOf`)