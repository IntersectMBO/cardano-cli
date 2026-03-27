{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Node.IssuePopBls where

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog.Extras qualified as H

hprop_golden_shelleyNodeIssuePopBls :: Property
hprop_golden_shelleyNodeIssuePopBls =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    signingKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/bls_keys/signing_key"
    goldenPopFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/bls_keys/signing_key.pop"
    popFile <- noteTempFile tempDir "bls.pop"

    void $
      execCardanoCLI
        [ "latest"
        , "node"
        , "issue-pop-BLS"
        , "--bls-signing-key-file"
        , signingKeyFile
        , "--out-file"
        , popFile
        ]

    H.diffFileVsGoldenFile popFile goldenPopFile
