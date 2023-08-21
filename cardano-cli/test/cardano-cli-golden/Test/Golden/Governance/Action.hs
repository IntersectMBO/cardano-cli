{- HLINT ignore "Use camelCase" -}

module Test.Golden.Governance.Action where

import           Control.Monad (void)

import qualified Test.Cardano.CLI.Util as H
import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Golden as H

hprop_golden_governanceActionCreateConstitution :: Property
hprop_golden_governanceActionCreateConstitution =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    stakeAddressVKeyFile <- noteTempFile tempDir "stake-address.vkey"
    stakeAddressSKeyFile <- noteTempFile tempDir "stake-address.skey"

    void $ execCardanoCLI
      [ "legacy", "stake-address", "key-gen"
      , "--verification-key-file", stakeAddressVKeyFile
      , "--signing-key-file", stakeAddressSKeyFile
      ]

    actionFile <- noteTempFile tempDir "create-constitution.action"
    redactedActionFile <- noteTempFile tempDir "create-constitution.action.redacted"

    void $ execCardanoCLI
      [ "conway", "governance", "action", "create-constitution"
      , "--governance-action-deposit", "10"
      , "--stake-verification-key-file", stakeAddressVKeyFile
      , "--out-file", actionFile
      , "--constitution", "This is a test constitution."
      ]

    goldenActionFile <-  H.note "test/cardano-cli-golden/files/golden/governance/action/create-constitution-for-stake-address.action.golden"

    H.redactJsonField "cborHex" "<cborHex>" actionFile redactedActionFile

    H.diffFileVsGoldenFile redactedActionFile goldenActionFile

    H.assertFileOccurences 1 "Governance proposal" actionFile

    H.assertEndsWithSingleNewline actionFile
