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
      , "--mainnet"
      , "--anchor-data-hash" , "eda258650888d4a7f8ac1127cfa136962f527f341c99db49929c79ae"
      , "--proposal-url" , "proposal-dummy-url"
      , "--governance-action-deposit", "10"
      , "--stake-verification-key-file", stakeAddressVKeyFile
      , "--out-file", actionFile
      , "--constitution-url", "constitution-dummy-url"
      , "--constitution", "This is a test constitution."
      ]

    goldenActionFile <-  H.note "test/cardano-cli-golden/files/golden/governance/action/create-constitution-for-stake-address.action.golden"

    H.redactJsonField "cborHex" "<cborHex>" actionFile redactedActionFile

    H.diffFileVsGoldenFile redactedActionFile goldenActionFile

    H.assertFileOccurences 1 "Governance proposal" actionFile

    H.assertEndsWithSingleNewline actionFile
