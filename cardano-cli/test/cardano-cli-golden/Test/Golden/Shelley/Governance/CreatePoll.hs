{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Governance.CreatePoll where

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_shelleyGovernanceCreatePoll :: Property
hprop_golden_shelleyGovernanceCreatePoll =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    pollFile <- noteTempFile tempDir "poll.json"

    stdout <-
      execCardanoCLI
        [ "legacy"
        , "governance"
        , "create-poll"
        , "--question"
        , "Pineapples on pizza?"
        , "--answer"
        , "yes"
        , "--answer"
        , "no"
        , "--out-file"
        , pollFile
        ]

    void $ H.readFile pollFile
    noteInputFile "test/cardano-cli-golden/files/golden/shelley/governance/create/basic.json"
      >>= H.readFile
      >>= (H.===) stdout
    H.assertFileOccurences 1 "GovernancePoll" pollFile
    H.assertEndsWithSingleNewline pollFile

hprop_golden_shelleyGovernanceCreateLongPoll :: Property
hprop_golden_shelleyGovernanceCreateLongPoll =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    pollFile <- noteTempFile tempDir "poll.json"

    stdout <-
      execCardanoCLI
        [ "legacy"
        , "governance"
        , "create-poll"
        , "--question"
        , "What is the most adequate topping to put on a pizza (please consider all possibilities and take time to answer)?"
        , "--answer"
        , "pineapples"
        , "--answer"
        , "only traditional topics should go on a pizza, this isn't room for jokes"
        , "--out-file"
        , pollFile
        ]

    void $ H.readFile pollFile
    noteInputFile "test/cardano-cli-golden/files/golden/shelley/governance/create/long-text.json"
      >>= H.readFile
      >>= (H.===) stdout
    H.assertFileOccurences 1 "GovernancePoll" pollFile
    H.assertEndsWithSingleNewline pollFile
