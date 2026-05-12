{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Debug.TransactionView where

import System.FilePath ((</>))

import Test.Cardano.CLI.Util (execCardanoCLI, watchdogProp)

import Hedgehog (Property)
import Hedgehog.Extras.Test (propertyOnce)
import Hedgehog.Extras.Test qualified as H

inputDir, goldenDir :: FilePath
inputDir = "test/cardano-cli-golden/files/input"
goldenDir = "test/cardano-cli-golden/files/golden"

-- ----------------------------------------------------------------------------
-- Conway era tests
-- ----------------------------------------------------------------------------

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden debug transaction view conway json/"'@
hprop_golden_debug_transaction_view_conway_json :: Property
hprop_golden_debug_transaction_view_conway_json =
  watchdogProp . propertyOnce $ do
    result <-
      execCardanoCLI
        [ "debug"
        , "transaction"
        , "view"
        , "--tx-file"
        , inputDir </> "conway/tx/signed.tx"
        , "--output-json"
        ]
    H.diffVsGoldenFile result $ goldenDir </> "conway/transaction-view.json"

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden debug transaction view conway yaml/"'@
hprop_golden_debug_transaction_view_conway_yaml :: Property
hprop_golden_debug_transaction_view_conway_yaml =
  watchdogProp . propertyOnce $ do
    result <-
      execCardanoCLI
        [ "debug"
        , "transaction"
        , "view"
        , "--tx-file"
        , inputDir </> "conway/tx/signed.tx"
        , "--output-yaml"
        ]
    H.diffVsGoldenFile result $ goldenDir </> "conway/transaction-view.yaml"
