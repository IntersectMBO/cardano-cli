{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE FlexibleContexts #-}

module Test.Golden.Governance.StakeAddress where

import Control.Monad (void)
import System.Exit (ExitCode (..))

import Test.Cardano.CLI.Hash (serveFilesWhile, tamperBase16Hash)
import Test.Cardano.CLI.Util
  ( execCardanoCLI
  , execDetailCardanoCLI
  , noteInputFile
  , propertyOnce
  , watchdogProp
  )

import Hedgehog
import Hedgehog qualified as H
import Hedgehog.Extras.Test qualified as H

exampleStakePoolMetadataHash :: String
exampleStakePoolMetadataHash = "8241de08075886a7d09c847c9bbd1719459dac0bd0a2f085e673611ebb9a5965"

exampleStakePoolMetadataPathGolden :: String
exampleStakePoolMetadataPathGolden = "test/cardano-cli-golden/files/input/example_stake_pool_metadata.json"

exampleStakePoolMetadataIpfsHash :: String
exampleStakePoolMetadataIpfsHash = "QmR1HAT4Hb4HjjqcgoXwupYXMF6t8h7MoSP24HMfV8t38a"

hprop_golden_conway_stakeaddress_delegate_no_confidence :: Property
hprop_golden_conway_stakeaddress_delegate_no_confidence =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
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
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
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
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
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
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
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
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
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
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
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
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
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
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
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

-- Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden stake pool metadata hash url wrong hash/"'@
hprop_golden_stake_pool_metadata_hash_url_wrong_hash :: Property
hprop_golden_stake_pool_metadata_hash_url_wrong_hash = do
  watchdogProp . propertyOnce $ do
    -- We modify the hash slightly so that the hash check fails
    alteredHash <- H.evalMaybe $ tamperBase16Hash exampleStakePoolMetadataHash
    let relativeUrl = [exampleStakePoolMetadataIpfsHash]

    -- Create temporary HTTP server with files required by the call to `cardano-cli`
    (exitCode, _, result) <-
      serveFilesWhile
        [ (relativeUrl, exampleStakePoolMetadataPathGolden)
        ]
        ( \port -> do
            execDetailCardanoCLI
              [ "conway"
              , "stake-pool"
              , "metadata-hash"
              , "--pool-metadata-url"
              , "http://127.0.0.1:" ++ show port ++ "/" ++ exampleStakePoolMetadataIpfsHash
              , "--expected-hash"
              , alteredHash
              ]
        )

    exitCode === ExitFailure 1

    H.diffVsGoldenFileExcludeTrace
      result
      "test/cardano-cli-golden/files/golden/governance/stakeaddress/stake_pool_metadata_hash_url_wrong_hash_fails.out"
