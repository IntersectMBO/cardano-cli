{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE TypeApplications #-}

module Test.Cli.Governance.Hash where

import           Control.Monad (forM_, void)
import           Text.Regex.TDFA ((=~))

import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

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
