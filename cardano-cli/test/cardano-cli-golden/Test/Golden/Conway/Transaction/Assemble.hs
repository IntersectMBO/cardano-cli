{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Conway.Transaction.Assemble where

import           Control.Monad (void)

import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Golden as H

{- HLINT ignore "Use camelCase" -}

-- Check that we can assemble a txbody and a tx witness to form a transaction

hprop_golden_conway_transaction_assemble_witness_signing_key :: Property
hprop_golden_conway_transaction_assemble_witness_signing_key = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  witnessTx <- noteTempFile tempDir "single-signing-key-witness-tx"
  txBodyFile <- noteInputFile "test/cardano-cli-golden/files/input/conway/txbody"
  signingKeyWitnessFile <-
    noteInputFile "test/cardano-cli-golden/files/input/conway/singleSigningKeyWitness"

  void $
    execCardanoCLI
      [ "conway"
      , "transaction"
      , "assemble"
      , "--tx-body-file"
      , txBodyFile
      , "--witness-file"
      , signingKeyWitnessFile
      , "--witness-file"
      , signingKeyWitnessFile
      , "--out-file"
      , witnessTx
      ]

  goldenFile <- H.note "test/cardano-cli-golden/files/golden/conway/transaction/assemble_out"
  H.diffFileVsGoldenFile witnessTx goldenFile
