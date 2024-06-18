module Test.Cli.Shelley.Transaction.Build where

import           Data.List (isInfixOf)
import           System.Exit (ExitCode (..))

import           Test.Cardano.CLI.Polysemy

import           Polysemy ()
import           HaskellWorks.Polysemy.Hedgehog
import           HaskellWorks.Prelude

inputDir :: FilePath
inputDir = "test/cardano-cli-test/files/input/shelley/transaction"

-- | This is a test of https://github.com/IntersectMBO/cardano-cli/issues/662
-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/conway transaction build one voter many votes/"'@
hprop_conway_transaction_build_one_voter_many_votes :: Property
hprop_conway_transaction_build_one_voter_many_votes = propertyOnce $ localWorkspace $ do
  outFile <- jotTempFile "tx.traw"

  (exitCode, _stdout, stderr) <- jotShowM $ execDetailCardanoCli
    [ "conway", "transaction", "build-raw"
    , "--tx-in", "6e8c947816e82627aeccb55300074f2894a2051332f62a1c8954e7b588a18be7#0"
    , "--tx-out", "addr_test1vpfwv0ezc5g8a4mkku8hhy3y3vp92t7s3ul8g778g5yegsgalc6gc+24910487859"
    , "--invalid-hereafter", "24325742"
    , "--fee" , "178569"
    , "--vote-file", inputDir </> "vote1.drep.json"
    , "--vote-file", inputDir </> "vote2.drep.json"
    , "--out-file", outFile
    ]

  exitCode === ExitFailure 1

  unless ("This would cause ignoring some of the votes" `isInfixOf` stderr) $ do
    jot_ stderr
    failure
