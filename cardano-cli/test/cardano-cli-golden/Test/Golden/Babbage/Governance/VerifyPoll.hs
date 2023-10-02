{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Babbage.Governance.VerifyPoll where

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

hprop_golden_babbageGovernanceVerifyPoll :: Property
hprop_golden_babbageGovernanceVerifyPoll = propertyOnce $ do
  pollFile <- noteInputFile "test/cardano-cli-golden/files/golden/babbage/governance/polls/basic.json"
  txFile <- noteInputFile "test/cardano-cli-golden/files/golden/babbage/governance/verify/valid"
  vkFile <- VerificationKeyFilePath . File <$>
    noteInputFile "test/cardano-cli-golden/files/golden/babbage/governance/cold.vk"

  stdout <- BSC.pack <$> execCardanoCLI
    [ "babbage", "governance", "verify-poll"
    , "--poll-file", pollFile
    , "--tx-file", txFile
    ]

  liftIO (readVerificationKeyOrTextEnvFile AsStakePoolKey vkFile) >>= \case
    Left e ->
      H.failWith Nothing (displayError e)
    Right vk -> do
      let expected = prettyPrintJSON $ serialiseToRawBytesHexText <$> [verificationKeyHash vk]
      H.assert $ expected `BSC.isInfixOf` stdout

hprop_golden_babbageGovernanceVerifyPollMismatch :: Property
hprop_golden_babbageGovernanceVerifyPollMismatch = propertyOnce $ do
  pollFile <- noteInputFile "test/cardano-cli-golden/files/golden/babbage/governance/polls/basic.json"
  txFile <- noteInputFile "test/cardano-cli-golden/files/golden/babbage/governance/verify/mismatch"

  result <- tryExecCardanoCLI
    [ "babbage", "governance", "verify-poll"
    , "--poll-file", pollFile
    , "--tx-file", txFile
    ]

  either (const H.success) (H.failWith Nothing) result

hprop_golden_babbageGovernanceVerifyPollNoAnswer :: Property
hprop_golden_babbageGovernanceVerifyPollNoAnswer = propertyOnce $ do
  pollFile <- noteInputFile "test/cardano-cli-golden/files/golden/babbage/governance/polls/basic.json"
  txFile <- noteInputFile "test/cardano-cli-golden/files/golden/babbage/governance/verify/none"

  result <- tryExecCardanoCLI
    [ "babbage", "governance", "verify-poll"
    , "--poll-file", pollFile
    , "--tx-file", txFile
    ]

  either (const H.success) (H.failWith Nothing) result

hprop_golden_babbageGovernanceVerifyPollMalformedAnswer :: Property
hprop_golden_babbageGovernanceVerifyPollMalformedAnswer = propertyOnce $ do
  pollFile <- noteInputFile "test/cardano-cli-golden/files/golden/babbage/governance/polls/basic.json"
  txFile <- noteInputFile "test/cardano-cli-golden/files/golden/babbage/governance/verify/malformed"

  result <- tryExecCardanoCLI
    [ "babbage", "governance", "verify-poll"
    , "--poll-file", pollFile
    , "--tx-file", txFile
    ]

  either (const H.success) (H.failWith Nothing) result

hprop_golden_babbageGovernanceVerifyPollInvalidAnswer :: Property
hprop_golden_babbageGovernanceVerifyPollInvalidAnswer = propertyOnce $ do
  pollFile <- noteInputFile "test/cardano-cli-golden/files/golden/babbage/governance/polls/basic.json"
  txFile <- noteInputFile "test/cardano-cli-golden/files/golden/babbage/governance/verify/invalid"

  result <- tryExecCardanoCLI
    [ "babbage", "governance", "verify-poll"
    , "--poll-file", pollFile
    , "--tx-file", txFile
    ]

  either (const H.success) (H.failWith Nothing) result
