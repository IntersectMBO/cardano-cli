{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Transaction.Assemble where

import Control.Monad (void)

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.File qualified as H

{- HLINT ignore "Use camelCase" -}

-- Check that we can assemble a txbody and a tx witness to form a transaction

hprop_golden_shelleyTransactionAssembleWitness_SigningKey :: Property
hprop_golden_shelleyTransactionAssembleWitness_SigningKey =
  propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    witnessTx <- noteTempFile tempDir "single-signing-key-witness-tx"
    txBodyFile <- noteInputFile "test/cardano-cli-golden/files/input/shelley/tx/txbody"
    signingKeyWitnessFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/witnesses/singleSigningKeyWitness"
    void $
      execCardanoCLI
        [ "latest"
        , "transaction"
        , "sign-witness"
        , "--tx-body-file"
        , txBodyFile
        , "--witness-file"
        , signingKeyWitnessFile
        , "--witness-file"
        , signingKeyWitnessFile
        , "--out-file"
        , witnessTx
        ]

    H.assertFileOccurences 1 "Tx MaryEra" witnessTx
