module Test.Golden.Hash.Hash where

import Control.Monad

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog.Extras qualified as H

hprop_golden_governance_hash_script :: Property
hprop_golden_governance_hash_script =
  watchdogProp . H.propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    scriptFile <- noteInputFile "test/cardano-cli-golden/files/input/hash/foo.script"
    hashFile <- H.noteTempFile tempDir "foo.script.hash"
    hashGold <- H.note "test/cardano-cli-golden/files/golden/hash/foo.script.hash"

    void $
      execCardanoCLI
        [ "hash"
        , "script"
        , "--script-file"
        , scriptFile
        , "--out-file"
        , hashFile
        ]

    H.diffFileVsGoldenFile hashFile hashGold

-- Check that `hash script` can hash a Plutus V4 script envelope.
-- Regression test for https://github.com/IntersectMBO/cardano-cli/issues/1448
hprop_golden_hash_script_plutus_v4 :: Property
hprop_golden_hash_script_plutus_v4 =
  watchdogProp . H.propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    scriptFile <-
      noteInputFile "test/cardano-cli-golden/files/input/dijkstra/plutus/v4-always-succeeds.plutus"
    hashFile <- H.noteTempFile tempDir "v4-always-succeeds.hash"
    hashGold <- H.note "test/cardano-cli-golden/files/golden/dijkstra/plutus/v4-always-succeeds.hash"

    void $
      execCardanoCLI
        [ "hash"
        , "script"
        , "--script-file"
        , scriptFile
        , "--out-file"
        , hashFile
        ]

    H.diffFileVsGoldenFile hashFile hashGold
