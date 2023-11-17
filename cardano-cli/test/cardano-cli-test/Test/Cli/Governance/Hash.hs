{- HLINT ignore "Use camelCase" -}

module Test.Cli.Governance.Hash where

import           Control.Monad (void)

import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

hprop_governance_hash_trip :: Property
hprop_governance_hash_trip =
  propertyOnce $ do
    governance_hash_trip_fun "foo"
    governance_hash_trip_fun "longerText"
    governance_hash_trip_fun "nonAscii: 你好"
    governance_hash_trip_fun "nonAscii: à la mode de Cæn"

-- Test that @cardano-cli conway governance hash --text > file1@ and
-- @cardano-cli conway governance hash --text --out-file file2@ yields
-- similar @file1@ and @file2@ files.
governance_hash_trip_fun :: String -> H.PropertyT IO ()
governance_hash_trip_fun input =
  H.moduleWorkspace "tmp" $ \tempDir -> do
    hashFile <- noteTempFile tempDir "hash.txt"

    hash <- execCardanoCLI
      [ "conway", "governance", "hash"
        , "--text", input
      ]

    void $ execCardanoCLI
      [ "conway", "governance", "hash"
      , "--text", input
      , "--out-file", hashFile
      ]

    hashFromFile <- H.readFile hashFile

    H.diff hash (==) hashFromFile
