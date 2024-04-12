{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE TypeApplications #-}

module Test.Cli.Governance.Hash where

import           Control.Monad (forM_, void)
import           Text.Regex.TDFA ((=~))

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
      [ "conway", "governance", "hash", "anchor-data"
        , "--text", input
      ]

    void $ execCardanoCLI
      [ "conway", "governance", "hash", "anchor-data"
      , "--text", input
      , "--out-file", hashFile
      ]

    hashFromFile <- H.readFile hashFile

    H.diff hash (==) hashFromFile

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/governance committee key hash/"'@
hprop_governance_committee_key_hash :: Property
hprop_governance_committee_key_hash =
  let supplyValues = [ "key-gen-cold", "key-gen-hot" ] in
  propertyOnce $ forM_ supplyValues $ \flag ->
    H.moduleWorkspace "tmp" $ \tempDir -> do
    verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
    signingKeyFile <- noteTempFile tempDir "key-gen.skey"

    void $ execCardanoCLI
      [ "conway", "governance", "committee", flag
      , "--verification-key-file", verificationKeyFile
      , "--signing-key-file", signingKeyFile
      ]

    result <- execCardanoCLI
      [  "conway", "governance", "committee", "key-hash"
      , "--verification-key-file", verificationKeyFile
      ]

    H.assert $ result =~ id @String "^[a-f0-9]{56}$"
