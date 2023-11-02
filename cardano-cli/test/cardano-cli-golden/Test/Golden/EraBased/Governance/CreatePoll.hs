{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.EraBased.Governance.CreatePoll where

import           Control.Monad (void)

import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Golden as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_governanceCreatePoll :: Property
hprop_golden_governanceCreatePoll =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    pollFile <- noteTempFile tempDir "poll.json"

    stdout <- execCardanoCLI
      [ "babbage", "governance", "create-poll"
      , "--question", "Pineapples on pizza?"
      , "--answer", "yes"
      , "--answer", "no"
      , "--out-file", pollFile
      ]

    void $ H.readFile pollFile
    noteInputFile "test/cardano-cli-golden/files/input/governance/create/basic.json"
      >>= H.readFile
      >>= (H.===) stdout

    goldenFile <- H.note "test/cardano-cli-golden/files/golden/governance/polls/create-poll-out.json"

    H.diffFileVsGoldenFile pollFile goldenFile

hprop_golden_governanceCreateLongPoll :: Property
hprop_golden_governanceCreateLongPoll =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    pollFile <- noteTempFile tempDir "poll.json"

    stdout <- execCardanoCLI
      [ "babbage", "governance", "create-poll"
      , "--question", "What is the most adequate topping to put on a pizza (please consider all possibilities and take time to answer)?"
      , "--answer", "pineapples"
      , "--answer", "only traditional topics should go on a pizza, this isn't room for jokes"
      , "--out-file", pollFile
      ]

    void $ H.readFile pollFile
    noteInputFile "test/cardano-cli-golden/files/input/governance/create/long-text.json"
      >>= H.readFile
      >>= (H.===) stdout

    goldenFile <- H.note "test/cardano-cli-golden/files/golden/governance/polls/create-poll-long-text-out.json"

    H.diffFileVsGoldenFile pollFile goldenFile
