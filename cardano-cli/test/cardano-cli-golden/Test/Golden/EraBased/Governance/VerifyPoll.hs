{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.EraBased.Governance.VerifyPoll where

import           Cardano.Api

import           Cardano.CLI.Types.Key (VerificationKeyOrFile (..),
                   readVerificationKeyOrTextEnvFile)

import qualified Data.ByteString.Char8 as BSC

import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Internal.Property as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_governanceVerifyPoll :: Property
hprop_golden_governanceVerifyPoll = propertyOnce $ do
  pollFile <- noteInputFile "test/cardano-cli-golden/files/input/governance/polls/basic.json"
  txFile <- noteInputFile "test/cardano-cli-golden/files/input/governance/verify/valid"
  goldenVkFile <-
    VerificationKeyFilePath . File
      <$> H.note "test/cardano-cli-golden/files/input/governance/cold.vk"

  stdout <-
    BSC.pack
      <$> execCardanoCLI
        [ "babbage"
        , "governance"
        , "verify-poll"
        , "--poll-file"
        , pollFile
        , "--tx-file"
        , txFile
        ]
  H.evalIO (runExceptT $ readVerificationKeyOrTextEnvFile AsStakePoolKey goldenVkFile) >>= \case
    Left e -> do
      H.noteShow_ $ prettyError e
      H.failure
    Right vk -> do
      let expected = prettyPrintJSON $ serialiseToRawBytesHexText <$> [verificationKeyHash vk]
      H.assert $ expected `BSC.isInfixOf` stdout

hprop_golden_governanceVerifyPollMismatch :: Property
hprop_golden_governanceVerifyPollMismatch = propertyOnce $ do
  pollFile <- noteInputFile "test/cardano-cli-golden/files/input/governance/polls/basic.json"
  txFile <- noteInputFile "test/cardano-cli-golden/files/input/governance/verify/mismatch"

  result <-
    tryExecCardanoCLI
      [ "babbage"
      , "governance"
      , "verify-poll"
      , "--poll-file"
      , pollFile
      , "--tx-file"
      , txFile
      ]

  either (const H.success) (H.failWith Nothing) result

hprop_golden_governanceVerifyPollNoAnswer :: Property
hprop_golden_governanceVerifyPollNoAnswer = propertyOnce $ do
  pollFile <- noteInputFile "test/cardano-cli-golden/files/input/governance/polls/basic.json"
  txFile <- noteInputFile "test/cardano-cli-golden/files/input/governance/verify/none"

  result <-
    tryExecCardanoCLI
      [ "babbage"
      , "governance"
      , "verify-poll"
      , "--poll-file"
      , pollFile
      , "--tx-file"
      , txFile
      ]

  either (const H.success) (H.failWith Nothing) result

hprop_golden_governanceVerifyPollMalformedAnswer :: Property
hprop_golden_governanceVerifyPollMalformedAnswer = propertyOnce $ do
  pollFile <- noteInputFile "test/cardano-cli-golden/files/input/governance/polls/basic.json"
  txFile <- noteInputFile "test/cardano-cli-golden/files/input/governance/verify/malformed"

  result <-
    tryExecCardanoCLI
      [ "babbage"
      , "governance"
      , "verify-poll"
      , "--poll-file"
      , pollFile
      , "--tx-file"
      , txFile
      ]

  either (const H.success) (H.failWith Nothing) result

hprop_golden_governanceVerifyPollInvalidAnswer :: Property
hprop_golden_governanceVerifyPollInvalidAnswer = propertyOnce $ do
  pollFile <- noteInputFile "test/cardano-cli-golden/files/input/governance/polls/basic.json"
  txFile <- noteInputFile "test/cardano-cli-golden/files/input/governance/verify/invalid"

  result <-
    tryExecCardanoCLI
      [ "babbage"
      , "governance"
      , "verify-poll"
      , "--poll-file"
      , pollFile
      , "--tx-file"
      , txFile
      ]

  either (const H.success) (H.failWith Nothing) result
