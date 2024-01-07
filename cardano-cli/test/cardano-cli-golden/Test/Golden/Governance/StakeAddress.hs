{- HLINT ignore "Use camelCase" -}

module Test.Golden.Governance.StakeAddress where

import           Control.Monad (void)

import           Test.Cardano.CLI.Util (execCardanoCLI, noteInputFile, propertyOnce)

import           Hedgehog
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Golden as H

hprop_golden_conway_stakeaddress_delegate_no_confidence :: Property
hprop_golden_conway_stakeaddress_delegate_no_confidence =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    vkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/conway/stake.vkey"
    delegFile <- H.noteTempFile tempDir "deleg"
    delegGold <- H.note "test/cardano-cli-golden/files/golden/governance/stakeaddress/noConfidenceDeleg.cert"

    void $ execCardanoCLI
      [ "conway", "stake-address", "vote-delegation-certificate"
      , "--stake-verification-key-file", vkeyFile
      , "--always-no-confidence"
      , "--out-file", delegFile
      ]

    H.diffFileVsGoldenFile delegFile delegGold

hprop_golden_conway_stakeaddress_delegate_always_abstain :: Property
hprop_golden_conway_stakeaddress_delegate_always_abstain =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    vkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/conway/stake.vkey"
    delegFile <- H.noteTempFile tempDir "deleg"
    delegGold <- H.note "test/cardano-cli-golden/files/golden/governance/stakeaddress/alwaysAbstainDeleg.cert"

    void $ execCardanoCLI
      [ "conway", "stake-address", "vote-delegation-certificate"
      , "--stake-verification-key-file", vkeyFile
      , "--always-abstain"
      , "--out-file", delegFile
      ]

    H.diffFileVsGoldenFile delegFile delegGold

hprop_golden_conway_stakeaddress_delegate_pool_and_no_confidence :: Property
hprop_golden_conway_stakeaddress_delegate_pool_and_no_confidence =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    vkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/conway/stake.vkey"
    vkeyPool <- noteInputFile "test/cardano-cli-golden/files/input/conway/poolCold.vkey"
    delegFile <- H.noteTempFile tempDir "deleg"
    delegGold <- H.note "test/cardano-cli-golden/files/golden/governance/stakeaddress/poolAndNoConfidenceDeleg.cert"

    void $ execCardanoCLI
      [ "conway", "stake-address", "stake-and-vote-delegation-certificate"
      , "--stake-verification-key-file", vkeyFile
      , "--cold-verification-key-file", vkeyPool
      , "--always-no-confidence"
      , "--out-file", delegFile
      ]

    H.diffFileVsGoldenFile delegFile delegGold

hprop_golden_conway_stakeaddress_delegate_pool_and_always_abstain :: Property
hprop_golden_conway_stakeaddress_delegate_pool_and_always_abstain =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    vkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/conway/stake.vkey"
    vkeyPool <- noteInputFile "test/cardano-cli-golden/files/input/conway/poolCold.vkey"
    delegFile <- H.noteTempFile tempDir "deleg"
    delegGold <- H.note "test/cardano-cli-golden/files/golden/governance/stakeaddress/poolAndAlwaysAbstainDeleg.cert"

    void $ execCardanoCLI
      [ "conway", "stake-address", "stake-and-vote-delegation-certificate"
      , "--stake-verification-key-file", vkeyFile
      , "--cold-verification-key-file", vkeyPool
      , "--always-abstain"
      , "--out-file", delegFile
      ]

    H.diffFileVsGoldenFile delegFile delegGold

hprop_golden_conway_stakeaddress_delegate_pool_and_drep :: Property
hprop_golden_conway_stakeaddress_delegate_pool_and_drep =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    vkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/conway/stake.vkey"
    vkeyPool <- noteInputFile "test/cardano-cli-golden/files/input/conway/poolCold.vkey"
    vkeyDrep <- noteInputFile "test/cardano-cli-golden/files/input/governance/drep/drep.vkey"
    delegFile <- H.noteTempFile tempDir "deleg"
    delegGold <- H.note "test/cardano-cli-golden/files/golden/governance/stakeaddress/poolAndDrepVkeyDeleg.cert"

    void $ execCardanoCLI
      [ "conway", "stake-address", "stake-and-vote-delegation-certificate"
      , "--stake-verification-key-file", vkeyFile
      , "--cold-verification-key-file", vkeyPool
      , "--drep-verification-key-file", vkeyDrep
      , "--out-file", delegFile
      ]

    H.diffFileVsGoldenFile delegFile delegGold
