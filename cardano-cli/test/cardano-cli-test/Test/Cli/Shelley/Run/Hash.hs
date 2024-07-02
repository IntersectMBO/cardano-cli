{- HLINT ignore "Use camelCase" -}

module Test.Cli.Shelley.Run.Hash where

import           Control.Monad (void)

import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

hprop_hash_trip :: Property
hprop_hash_trip =
  propertyOnce $ do
    hash_trip_fun "foo"
    hash_trip_fun "longerText"
    hash_trip_fun "nonAscii: 你好"
    hash_trip_fun "nonAscii: à la mode de Cæn"

-- Test that @cardano-cli hash --text > file1@ and
-- @cardano-cli --text --out-file file2@ yields
-- similar @file1@ and @file2@ files.
hash_trip_fun :: String -> H.PropertyT IO ()
hash_trip_fun input =
  H.moduleWorkspace "tmp" $ \tempDir -> do
    hashFile <- noteTempFile tempDir "hash.txt"

    hash <- execCardanoCLI
      [ "hash", "anchor-data"
        , "--text", input
      ]

    void $ execCardanoCLI
      [ "hash", "anchor-data"
      , "--text", input
      , "--out-file", hashFile
      ]

    hashFromFile <- H.readFile hashFile

    H.diff hash (==) hashFromFile
