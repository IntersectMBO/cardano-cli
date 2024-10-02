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
    delegGold <-
      H.note "test/cardano-cli-golden/files/golden/governance/stakeaddress/noConfidenceDeleg.cert"

    void $
      execCardanoCLI
        [ "conway"
        , "stake-address"
        , "vote-delegation-certificate"
        , "--stake-verification-key-file"
        , vkeyFile
        , "--always-no-confidence"
        , "--out-file"
        , delegFile
        ]

    H.diffFileVsGoldenFile delegFile delegGold

hprop_golden_conway_stakeaddress_delegate_always_abstain :: Property
hprop_golden_conway_stakeaddress_delegate_always_abstain =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    vkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/conway/stake.vkey"
    delegFile <- H.noteTempFile tempDir "deleg"
    delegGold <-
      H.note "test/cardano-cli-golden/files/golden/governance/stakeaddress/alwaysAbstainDeleg.cert"

    void $
      execCardanoCLI
        [ "conway"
        , "stake-address"
        , "vote-delegation-certificate"
        , "--stake-verification-key-file"
        , vkeyFile
        , "--always-abstain"
        , "--out-file"
        , delegFile
        ]

    H.diffFileVsGoldenFile delegFile delegGold

hprop_golden_conway_stakeaddress_delegate_pool_and_no_confidence :: Property
hprop_golden_conway_stakeaddress_delegate_pool_and_no_confidence =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    vkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/conway/stake.vkey"
    vkeyPool <- noteInputFile "test/cardano-cli-golden/files/input/conway/poolCold.vkey"
    delegFile <- H.noteTempFile tempDir "deleg"
    delegGold <-
      H.note "test/cardano-cli-golden/files/golden/governance/stakeaddress/poolAndNoConfidenceDeleg.cert"

    void $
      execCardanoCLI
        [ "conway"
        , "stake-address"
        , "stake-and-vote-delegation-certificate"
        , "--stake-verification-key-file"
        , vkeyFile
        , "--cold-verification-key-file"
        , vkeyPool
        , "--always-no-confidence"
        , "--out-file"
        , delegFile
        ]

    H.diffFileVsGoldenFile delegFile delegGold

hprop_golden_conway_stakeaddress_delegate_pool_and_always_abstain :: Property
hprop_golden_conway_stakeaddress_delegate_pool_and_always_abstain =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    vkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/conway/stake.vkey"
    vkeyPool <- noteInputFile "test/cardano-cli-golden/files/input/conway/poolCold.vkey"
    delegFile <- H.noteTempFile tempDir "deleg"
    delegGold <-
      H.note "test/cardano-cli-golden/files/golden/governance/stakeaddress/poolAndAlwaysAbstainDeleg.cert"

    void $
      execCardanoCLI
        [ "conway"
        , "stake-address"
        , "stake-and-vote-delegation-certificate"
        , "--stake-verification-key-file"
        , vkeyFile
        , "--cold-verification-key-file"
        , vkeyPool
        , "--always-abstain"
        , "--out-file"
        , delegFile
        ]

    H.diffFileVsGoldenFile delegFile delegGold

hprop_golden_conway_stakeaddress_delegate_pool_and_drep :: Property
hprop_golden_conway_stakeaddress_delegate_pool_and_drep =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    vkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/conway/stake.vkey"
    vkeyPool <- noteInputFile "test/cardano-cli-golden/files/input/conway/poolCold.vkey"
    vkeyDrep <- noteInputFile "test/cardano-cli-golden/files/input/governance/drep/drep.vkey"
    delegFile <- H.noteTempFile tempDir "deleg"
    delegGold <-
      H.note "test/cardano-cli-golden/files/golden/governance/stakeaddress/poolAndDrepVkeyDeleg.cert"

    void $
      execCardanoCLI
        [ "conway"
        , "stake-address"
        , "stake-and-vote-delegation-certificate"
        , "--stake-verification-key-file"
        , vkeyFile
        , "--cold-verification-key-file"
        , vkeyPool
        , "--drep-verification-key-file"
        , vkeyDrep
        , "--out-file"
        , delegFile
        ]

    H.diffFileVsGoldenFile delegFile delegGold

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden conway stakeaddress register and delegate pool/"'@
hprop_golden_conway_stakeaddress_register_and_delegate_pool :: Property
hprop_golden_conway_stakeaddress_register_and_delegate_pool =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    vkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/conway/stake.vkey"
    vkeyPool <- noteInputFile "test/cardano-cli-golden/files/input/conway/poolCold.vkey"
    certFile <- H.noteTempFile tempDir "cert"
    certGold <-
      H.note
        "test/cardano-cli-golden/files/golden/governance/stakeaddress/registerAddressDelegateToPool.cert"

    void $
      execCardanoCLI
        [ "conway"
        , "stake-address"
        , "registration-and-delegation-certificate"
        , "--stake-verification-key-file"
        , vkeyFile
        , "--cold-verification-key-file"
        , vkeyPool
        , "--key-reg-deposit-amt"
        , "2000000"
        , "--out-file"
        , certFile
        ]

    H.diffFileVsGoldenFile certFile certGold

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden conway stakeaddress register and delegate vote/"'@
hprop_golden_conway_stakeaddress_register_and_delegate_vote :: Property
hprop_golden_conway_stakeaddress_register_and_delegate_vote =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    vkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/conway/stake.vkey"
    vkeyDrepFile <- noteInputFile "test/cardano-cli-golden/files/input/governance/drep/drep.vkey"
    certFile <- H.noteTempFile tempDir "cert"
    certGold <-
      H.note
        "test/cardano-cli-golden/files/golden/governance/stakeaddress/registerAddressDelegateToDrep.cert"

    void $
      execCardanoCLI
        [ "conway"
        , "stake-address"
        , "registration-and-vote-delegation-certificate"
        , "--stake-verification-key-file"
        , vkeyFile
        , "--drep-verification-key-file"
        , vkeyDrepFile
        , "--key-reg-deposit-amt"
        , "2000000"
        , "--out-file"
        , certFile
        ]

    H.diffFileVsGoldenFile certFile certGold

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden conway stakeaddress register and delegate stake and vote/"'@
hprop_golden_conway_stakeaddress_register_and_delegate_stake_and_vote :: Property
hprop_golden_conway_stakeaddress_register_and_delegate_stake_and_vote =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    vkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/conway/stake.vkey"
    vkeyPool <- noteInputFile "test/cardano-cli-golden/files/input/conway/poolCold.vkey"
    vkeyDrepFile <- noteInputFile "test/cardano-cli-golden/files/input/governance/drep/drep.vkey"
    certFile <- H.noteTempFile tempDir "cert"
    certGold <-
      H.note
        "test/cardano-cli-golden/files/golden/governance/stakeaddress/registerAddressDelegateToPoolAndDrep.cert"

    void $
      execCardanoCLI
        [ "conway"
        , "stake-address"
        , "registration-stake-and-vote-delegation-certificate"
        , "--stake-verification-key-file"
        , vkeyFile
        , "--cold-verification-key-file"
        , vkeyPool
        , "--drep-verification-key-file"
        , vkeyDrepFile
        , "--key-reg-deposit-amt"
        , "2000000"
        , "--out-file"
        , certFile
        ]

    H.diffFileVsGoldenFile certFile certGold
