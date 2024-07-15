module Test.Cli.Shelley.Transaction.Build where

import           Data.List (isInfixOf)
import           System.Exit (ExitCode (..))
import           System.FilePath ((</>))

import           Test.Cardano.CLI.Util

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H

inputDir :: FilePath
inputDir = "test/cardano-cli-test/files/input/shelley/transaction"

-- | This is a test of https://github.com/IntersectMBO/cardano-cli/issues/662
-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/conway transaction build one voter many votes/"'@
hprop_conway_transaction_build_one_voter_many_votes :: Property
hprop_conway_transaction_build_one_voter_many_votes = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
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
