{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Test.Golden.Governance.Hash where
import           Control.Monad

import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Test.Golden as H


hprop_golden_governance_hash_script :: Property
hprop_golden_governance_hash_script =
  H.propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    scriptFile <- noteInputFile "test/cardano-cli-golden/files/input/governance/hash/foo.script"
    hashFile <- H.noteTempFile tempDir "foo.script.hash"
    hashGold <- H.note "test/cardano-cli-golden/files/golden/governance/hash/foo.script.hash"

    void $ execCardanoCLI
      [ "conway", "governance", "hash", "script"
      , "--script-file", scriptFile
      , "--out-file", hashFile
      ]

    H.diffFileVsGoldenFile hashFile hashGold
