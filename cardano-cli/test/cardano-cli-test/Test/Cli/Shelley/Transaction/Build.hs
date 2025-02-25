module Test.Cli.Shelley.Transaction.Build where

import Data.List (isInfixOf)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))

import Test.Cardano.CLI.Util

import Hedgehog
import Hedgehog qualified as H
import Hedgehog.Extras.Test.Base qualified as H

inputDir :: FilePath
inputDir = "test/cardano-cli-test/files/input/shelley/transaction"

-- | This is a test of https://github.com/IntersectMBO/cardano-cli/issues/662
-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/conway transaction build one voter many votes/"'@
hprop_conway_transaction_build_one_voter_many_votes :: Property
hprop_conway_transaction_build_one_voter_many_votes = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  outFile <- H.noteTempFile tempDir "tx.traw"

  (exitCode, _stdout, stderr) <-
    H.noteShowM $
      execDetailCardanoCLI
        [ "conway"
        , "transaction"
        , "build-raw"
        , "--tx-in"
        , "6e8c947816e82627aeccb55300074f2894a2051332f62a1c8954e7b588a18be7#0"
        , "--tx-out"
        , "addr_test1vpfwv0ezc5g8a4mkku8hhy3y3vp92t7s3ul8g778g5yegsgalc6gc+24910487859"
        , "--invalid-hereafter"
        , "24325742"
        , "--fee"
        , "178569"
        , "--vote-file"
        , inputDir </> "vote1.drep.json"
        , "--vote-file"
        , inputDir </> "vote2.drep.json"
        , "--out-file"
        , outFile
        ]

  exitCode H.=== ExitFailure 1
  H.assertWith stderr ("This would cause ignoring some of the votes" `isInfixOf`)

-- | This is a test of https://github.com/IntersectMBO/cardano-cli/issues/904
-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/conway transaction build raw negative txout/"'@
hprop_conway_transaction_build_raw_negative_txout :: Property
hprop_conway_transaction_build_raw_negative_txout = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  outFile <- H.noteTempFile tempDir "tx.traw"

  (exitCode, _stdout, stderr) <-
    H.noteShowM $
      execDetailCardanoCLI
        [ "conway"
        , "transaction"
        , "build-raw"
        , "--fee"
        , "200000"
        , "--tx-in"
        , "e25450233e4bedd00c8bda15c48c2d4018223bd88271e194052294c4e5be7d55#0"
        , "--tx-out"
        , "addr_test1vqfxq2s8yce3tuhjq9ulu2awuk623hzvtft9z8fh6qelzts49vuqw+-1" -- This is the negative txout
        , "--out-file"
        , outFile
        ]

  exitCode H.=== ExitFailure 1
  H.assertWith stderr ("Value must be positive in UTxO (or transaction output)" `isInfixOf`)

-- | This is a test that we allow transaction outputs to contain negative bits, if
-- the grand total is positive.
-- @cabal test cardano-cli-test --test-options '-p "/conway transaction build raw negative bits positive total txout/"'@
hprop_conway_transaction_build_raw_negative_bits_positive_total_txout :: Property
hprop_conway_transaction_build_raw_negative_bits_positive_total_txout = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  outFile <- H.noteTempFile tempDir "tx.traw"

  -- This checks that the command succeeds
  H.noteShowM_ $
    execCardanoCLI
      [ "conway"
      , "transaction"
      , "build-raw"
      , "--fee"
      , "200000"
      , "--tx-in"
      , "e25450233e4bedd00c8bda15c48c2d4018223bd88271e194052294c4e5be7d55#0"
      , "--tx-out"
      , "addr_test1vqfxq2s8yce3tuhjq9ulu2awuk623hzvtft9z8fh6qelzts49vuqw+-1+3" -- Negative txout (-1) summed with positive txout (+3), so total is positive
      , "--out-file"
      , outFile
      ]
