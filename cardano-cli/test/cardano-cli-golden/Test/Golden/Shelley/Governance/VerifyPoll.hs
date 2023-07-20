{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Governance.VerifyPoll
  ( golden_shelleyGovernanceVerifyPoll
  , golden_shelleyGovernanceVerifyPollMismatch
  , golden_shelleyGovernanceVerifyPollNoAnswer
  , golden_shelleyGovernanceVerifyPollMalformedAnswer
  , golden_shelleyGovernanceVerifyPollInvalidAnswer
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Key (VerificationKeyOrFile (..),
                   readVerificationKeyOrTextEnvFile)

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BSC

import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Internal.Property as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyGovernanceVerifyPoll :: Property
golden_shelleyGovernanceVerifyPoll = propertyOnce $ do
  pollFile <- noteInputFile "test/cardano-cli-golden/files/golden/shelley/governance/polls/basic.json"
  txFile <- noteInputFile "test/cardano-cli-golden/files/golden/shelley/governance/verify/valid"
  vkFile <- VerificationKeyFilePath . File <$>
    noteInputFile "test/cardano-cli-golden/files/golden/shelley/governance/cold.vk"

  stdout <- BSC.pack <$> execCardanoCLI
    [ "legacy", "governance", "verify-poll"
    , "--poll-file", pollFile
    , "--tx-file", txFile
    ]

  liftIO (readVerificationKeyOrTextEnvFile AsStakePoolKey vkFile) >>= \case
    Left e ->
      H.failWith Nothing (displayError e)
    Right vk -> do
      let expected = prettyPrintJSON $ serialiseToRawBytesHexText <$> [verificationKeyHash vk]
      H.assert $ expected `BSC.isInfixOf` stdout

golden_shelleyGovernanceVerifyPollMismatch :: Property
golden_shelleyGovernanceVerifyPollMismatch = propertyOnce $ do
  pollFile <- noteInputFile "test/cardano-cli-golden/files/golden/shelley/governance/polls/basic.json"
  txFile <- noteInputFile "test/cardano-cli-golden/files/golden/shelley/governance/verify/mismatch"

  result <- tryExecCardanoCLI
    [ "legacy", "governance", "verify-poll"
    , "--poll-file", pollFile
    , "--tx-file", txFile
    ]

  either (const H.success) (H.failWith Nothing) result

golden_shelleyGovernanceVerifyPollNoAnswer :: Property
golden_shelleyGovernanceVerifyPollNoAnswer = propertyOnce $ do
  pollFile <- noteInputFile "test/cardano-cli-golden/files/golden/shelley/governance/polls/basic.json"
  txFile <- noteInputFile "test/cardano-cli-golden/files/golden/shelley/governance/verify/none"

  result <- tryExecCardanoCLI
    [ "legacy", "governance", "verify-poll"
    , "--poll-file", pollFile
    , "--tx-file", txFile
    ]

  either (const H.success) (H.failWith Nothing) result

golden_shelleyGovernanceVerifyPollMalformedAnswer :: Property
golden_shelleyGovernanceVerifyPollMalformedAnswer = propertyOnce $ do
  pollFile <- noteInputFile "test/cardano-cli-golden/files/golden/shelley/governance/polls/basic.json"
  txFile <- noteInputFile "test/cardano-cli-golden/files/golden/shelley/governance/verify/malformed"

  result <- tryExecCardanoCLI
    [ "legacy", "governance", "verify-poll"
    , "--poll-file", pollFile
    , "--tx-file", txFile
    ]

  either (const H.success) (H.failWith Nothing) result

golden_shelleyGovernanceVerifyPollInvalidAnswer :: Property
golden_shelleyGovernanceVerifyPollInvalidAnswer = propertyOnce $ do
  pollFile <- noteInputFile "test/cardano-cli-golden/files/golden/shelley/governance/polls/basic.json"
  txFile <- noteInputFile "test/cardano-cli-golden/files/golden/shelley/governance/verify/invalid"

  result <- tryExecCardanoCLI
    [ "legacy", "governance", "verify-poll"
    , "--poll-file", pollFile
    , "--tx-file", txFile
    ]

  either (const H.success) (H.failWith Nothing) result
