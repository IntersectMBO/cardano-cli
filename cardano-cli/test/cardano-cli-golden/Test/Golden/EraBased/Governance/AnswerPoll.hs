{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.EraBased.Governance.AnswerPoll where

import           Control.Monad (void)

import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Golden as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_governanceAnswerPollNeg1Invalid :: Property
hprop_golden_governanceAnswerPollNeg1Invalid = propertyOnce . H.moduleWorkspace "governance-answer-poll" $ \tempDir -> do
  pollFile <- noteInputFile "test/cardano-cli-golden/files/input/governance/polls/basic.json"
  outFile <- H.noteTempFile tempDir "answer-file.json"

  result <-
    tryExecCardanoCLI
      [ "babbage"
      , "governance"
      , "answer-poll"
      , "--poll-file"
      , pollFile
      , "--answer"
      , "-1"
      , "--out-file"
      , outFile
      ]

  H.assertFileMissing outFile

  either (const H.success) (const H.failure) result

hprop_golden_governanceAnswerPoll0 :: Property
hprop_golden_governanceAnswerPoll0 = propertyOnce . H.moduleWorkspace "governance-answer-poll" $ \tempDir -> do
  pollFile <- noteInputFile "test/cardano-cli-golden/files/input/governance/polls/basic.json"
  goldenAnswerFile <-
    H.note "test/cardano-cli-golden/files/golden/governance/polls/basic.answer.0.json"
  outFile <- H.noteTempFile tempDir "answer-file.json"

  void $
    execCardanoCLI
      [ "babbage"
      , "governance"
      , "answer-poll"
      , "--poll-file"
      , pollFile
      , "--answer"
      , "0"
      , "--out-file"
      , outFile
      ]

  H.diffFileVsGoldenFile outFile goldenAnswerFile

hprop_golden_governanceAnswerPollPos1 :: Property
hprop_golden_governanceAnswerPollPos1 = propertyOnce . H.moduleWorkspace "governance-answer-poll" $ \tempDir -> do
  pollFile <- noteInputFile "test/cardano-cli-golden/files/input/governance/polls/basic.json"
  goldenAnswerFile <-
    H.note "test/cardano-cli-golden/files/golden/governance/polls/basic.answer.1.json"
  outFile <- H.noteTempFile tempDir "answer-file.json"

  void $
    execCardanoCLI
      [ "babbage"
      , "governance"
      , "answer-poll"
      , "--poll-file"
      , pollFile
      , "--answer"
      , "1"
      , "--out-file"
      , outFile
      ]

  H.diffFileVsGoldenFile outFile goldenAnswerFile

hprop_golden_governanceAnswerPollPos2Invalid :: Property
hprop_golden_governanceAnswerPollPos2Invalid = propertyOnce . H.moduleWorkspace "governance-answer-poll" $ \tempDir -> do
  pollFile <- noteInputFile "test/cardano-cli-golden/files/input/governance/polls/basic.json"
  outFile <- H.noteTempFile tempDir "answer-file.json"

  result <-
    tryExecCardanoCLI
      [ "babbage"
      , "governance"
      , "answer-poll"
      , "--poll-file"
      , pollFile
      , "--answer"
      , "2"
      , "--out-file"
      , outFile
      ]

  H.assertFileMissing outFile

  either (const H.success) (const H.failure) result
