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
-- Shelley era tests
-- ----------------------------------------------------------------------------

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden debug transaction view shelley json/"'@
hprop_golden_debug_transaction_view_shelley_json :: Property
hprop_golden_debug_transaction_view_shelley_json =
  watchdogProp . propertyOnce $ do
    result <-
      execCardanoCLI
        [ "debug"
        , "transaction"
        , "view"
        , "--tx-file"
        , inputDir </> "shelley/tx/signed.tx"
        , "--output-json"
        ]
    H.diffVsGoldenFile result $ goldenDir </> "shelley/transaction-view.json"

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden debug transaction view shelley yaml/"'@
hprop_golden_debug_transaction_view_shelley_yaml :: Property
hprop_golden_debug_transaction_view_shelley_yaml =
  watchdogProp . propertyOnce $ do
    result <-
      execCardanoCLI
        [ "debug"
        , "transaction"
        , "view"
        , "--tx-file"
        , inputDir </> "shelley/tx/signed.tx"
        , "--output-yaml"
        ]
    H.diffVsGoldenFile result $ goldenDir </> "shelley/transaction-view.yaml"

-- ----------------------------------------------------------------------------
-- Allegra era tests
-- ----------------------------------------------------------------------------

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden debug transaction view allegra json/"'@
hprop_golden_debug_transaction_view_allegra_json :: Property
hprop_golden_debug_transaction_view_allegra_json =
  watchdogProp . propertyOnce $ do
    result <-
      execCardanoCLI
        [ "debug"
        , "transaction"
        , "view"
        , "--tx-file"
        , inputDir </> "allegra/tx/signed.tx"
        , "--output-json"
        ]
    H.diffVsGoldenFile result $ goldenDir </> "allegra/transaction-view.json"

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden debug transaction view allegra yaml/"'@
hprop_golden_debug_transaction_view_allegra_yaml :: Property
hprop_golden_debug_transaction_view_allegra_yaml =
  watchdogProp . propertyOnce $ do
    result <-
      execCardanoCLI
        [ "debug"
        , "transaction"
        , "view"
        , "--tx-file"
        , inputDir </> "allegra/tx/signed.tx"
        , "--output-yaml"
        ]
    H.diffVsGoldenFile result $ goldenDir </> "allegra/transaction-view.yaml"

-- ----------------------------------------------------------------------------
-- Mary era tests
-- ----------------------------------------------------------------------------

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden debug transaction view mary json/"'@
hprop_golden_debug_transaction_view_mary_json :: Property
hprop_golden_debug_transaction_view_mary_json =
  watchdogProp . propertyOnce $ do
    result <-
      execCardanoCLI
        [ "debug"
        , "transaction"
        , "view"
        , "--tx-file"
        , inputDir </> "mary/tx/signed.tx"
        , "--output-json"
        ]
    H.diffVsGoldenFile result $ goldenDir </> "mary/transaction-view.json"

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden debug transaction view mary yaml/"'@
hprop_golden_debug_transaction_view_mary_yaml :: Property
hprop_golden_debug_transaction_view_mary_yaml =
  watchdogProp . propertyOnce $ do
    result <-
      execCardanoCLI
        [ "debug"
        , "transaction"
        , "view"
        , "--tx-file"
        , inputDir </> "mary/tx/signed.tx"
        , "--output-yaml"
        ]
    H.diffVsGoldenFile result $ goldenDir </> "mary/transaction-view.yaml"

-- ----------------------------------------------------------------------------
-- Alonzo era tests
-- ----------------------------------------------------------------------------

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden debug transaction view alonzo json/"'@
hprop_golden_debug_transaction_view_alonzo_json :: Property
hprop_golden_debug_transaction_view_alonzo_json =
  watchdogProp . propertyOnce $ do
    result <-
      execCardanoCLI
        [ "debug"
        , "transaction"
        , "view"
        , "--tx-file"
        , inputDir </> "alonzo/tx/signed.tx"
        , "--output-json"
        ]
    H.diffVsGoldenFile result $ goldenDir </> "alonzo/transaction-view.json"

-- ----------------------------------------------------------------------------
-- Babbage era tests
-- ----------------------------------------------------------------------------

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden debug transaction view babbage json/"'@
hprop_golden_debug_transaction_view_babbage_json :: Property
hprop_golden_debug_transaction_view_babbage_json =
  watchdogProp . propertyOnce $ do
    result <-
      execCardanoCLI
        [ "debug"
        , "transaction"
        , "view"
        , "--tx-file"
        , inputDir </> "babbage/tx/signed.tx"
        , "--output-json"
        ]
    H.diffVsGoldenFile result $ goldenDir </> "babbage/transaction-view.json"

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden debug transaction view babbage yaml/"'@
hprop_golden_debug_transaction_view_babbage_yaml :: Property
hprop_golden_debug_transaction_view_babbage_yaml =
  watchdogProp . propertyOnce $ do
    result <-
      execCardanoCLI
        [ "debug"
        , "transaction"
        , "view"
        , "--tx-file"
        , inputDir </> "babbage/tx/signed.tx"
        , "--output-yaml"
        ]
    H.diffVsGoldenFile result $ goldenDir </> "babbage/transaction-view.yaml"

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
