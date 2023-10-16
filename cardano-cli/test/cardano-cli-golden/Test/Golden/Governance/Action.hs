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
      , "--proposal-anchor-metadata", "eda258650888d4a7f8ac1127cfa136962f527f341c99db49929c79ae"
      , "--proposal-anchor-url", "proposal-dummy-url"
      , "--governance-action-deposit", "10"
      , "--stake-verification-key-file", stakeAddressVKeyFile
      , "--out-file", actionFile
      , "--constitution-anchor-url", "constitution-dummy-url"
      , "--constitution-anchor-metadata", "This is a test constitution."
      ]

    goldenActionFile <-  H.note "test/cardano-cli-golden/files/golden/governance/action/create-constitution-for-stake-address.action.golden"

    H.redactJsonField "cborHex" "<cborHex>" actionFile redactedActionFile

    H.diffFileVsGoldenFile redactedActionFile goldenActionFile

    H.assertFileOccurences 1 "Governance proposal" actionFile

    H.assertEndsWithSingleNewline actionFile

hprop_golden_conway_governance_action_view_constitution_json :: Property
hprop_golden_conway_governance_action_view_constitution_json =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    stakeAddressVKeyFile <- H.note "test/cardano-cli-golden/files/input/governance/stake-address.vkey"

    actionFile <- noteTempFile tempDir "action"

    void $ execCardanoCLI
      [ "conway", "governance", "action", "create-constitution"
      , "--mainnet"
      , "--proposal-anchor-metadata", "eda258650888d4a7f8ac1127cfa136962f527f341c99db49929c79ae"
      , "--proposal-anchor-url", "proposal-dummy-url"
      , "--governance-action-deposit", "10"
      , "--stake-verification-key-file", stakeAddressVKeyFile
      , "--out-file", actionFile
      , "--constitution-anchor-url", "constitution-dummy-url"
      , "--constitution-anchor-metadata", "This is a test constitution."
      ]

    goldenActionViewFile <- H.note "test/cardano-cli-golden/files/golden/governance/action/view/create-constitution.action.view"
    actionView <- execCardanoCLI
      [ "conway", "governance", "action", "view"
      , "--action-file", actionFile
      ]
    H.diffVsGoldenFile actionView goldenActionViewFile

hprop_golden_conway_governance_action_view_update_committee_yaml :: Property
hprop_golden_conway_governance_action_view_update_committee_yaml =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    stakeAddressVKeyFile <- H.note "test/cardano-cli-golden/files/input/governance/stake-address.vkey"

    actionFile <- noteTempFile tempDir "action"

    void $ execCardanoCLI
      [ "conway", "governance", "action", "update-committee"
      , "--mainnet"
      , "--governance-action-deposit", "10"
      , "--stake-verification-key-file", stakeAddressVKeyFile
      , "--proposal-anchor-url", "proposal-dummy-url"
      , "--proposal-anchor-metadata", "eda258650888d4a7f8ac1127cfa136962f527f341c99db49929c79ae"
      , "--quorum", "0.61"
      , "--out-file", actionFile
      ]

    goldenActionViewFile <- H.note "test/cardano-cli-golden/files/golden/governance/action/view/update-committee.action.view"
    actionView <- execCardanoCLI
      [ "conway", "governance", "action", "view"
      , "--action-file", actionFile
      , "--output-format", "yaml"
      ]
    H.diffVsGoldenFile actionView goldenActionViewFile

hprop_golden_conway_governance_action_view_create_info_json_outfile :: Property
hprop_golden_conway_governance_action_view_create_info_json_outfile =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    stakeAddressVKeyFile <- H.note "test/cardano-cli-golden/files/input/governance/stake-address.vkey"

    actionFile <- noteTempFile tempDir "action"

    void $ execCardanoCLI
      [ "conway", "governance", "action", "create-info"
      , "--testnet"
      , "--governance-action-deposit", "10"
      , "--stake-verification-key-file", stakeAddressVKeyFile
      , "--proposal-anchor-url", "proposal-dummy-url"
      , "--proposal-anchor-metadata", "eda258650888d4a7f8ac1127cfa136962f527f341c99db49929c79ae"
      , "--out-file", actionFile
      ]

    actionViewFile <- noteTempFile tempDir "action-view"
    goldenActionViewFile <- H.note "test/cardano-cli-golden/files/golden/governance/action/view/create-info.action.view"
    void $ execCardanoCLI
      [ "conway", "governance", "action", "view"
      , "--action-file", actionFile
      , "--out-file", actionViewFile
      ]
    H.diffFileVsGoldenFile actionViewFile goldenActionViewFile
