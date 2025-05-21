{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.TxView where

import Cardano.Api (TxMetadataJsonSchema (..))

import Control.Monad (void)
import System.FilePath ((</>))

import Test.Cardano.CLI.Util (execCardanoCLI, noteTempFile, watchdogProp)

import Hedgehog (Property)
import Hedgehog.Extras (Integration, moduleWorkspace, propertyOnce)
import Hedgehog.Extras.Test.Golden qualified as H

goldenDir, inputDir :: FilePath
goldenDir = "test/cardano-cli-golden/files/golden"
inputDir = "test/cardano-cli-golden/files/input"

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden view babbage yaml/"'@
hprop_golden_view_babbage_yaml :: Property
hprop_golden_view_babbage_yaml =
  watchdogProp . propertyOnce $
    moduleWorkspace "tmp" $ \tempDir -> do
      transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

      -- Create transaction body
      void $
        execCardanoCLI
          [ "conway"
          , "transaction"
          , "build-raw"
          , -- Txin with Plutus script
            "--tx-in"
          , "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891#135"
          , "--tx-in-script-file"
          , inputDir </> "AlwaysSucceeds.plutus"
          , "--tx-in-datum-value"
          , "24"
          , "--tx-in-redeemer-value"
          , "42"
          , "--tx-in-execution-units"
          , "(100,110)"
          , -- Txin with native script
            "--tx-in"
          , "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891#136"
          , "--tx-in-script-file"
          , inputDir </> "NativeScript.json"
          , -- Txin with ref script
            "--tx-in"
          , "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891#137"
          , "--simple-script-tx-in-reference"
          , "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891#138"
          , -- Read only ref
            "--read-only-tx-in-reference"
          , "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891#139"
          , -- Collateral
            "--tx-in-collateral"
          , "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891#140"
          , -- Excess collateral output
            "--tx-out-return-collateral"
          , mconcat
              [ "addr_test1"
              , "qrefnr4k09pvge6dq83v6s67ruter8sftmky8qrmkqqsxy7q5psgn8tgqmupq4r7"
              , "9jmxlyk4eqt6z6hj5g8jd8393msqaw47f4"
              , " + "
              , "200"
              ]
          , "--tx-total-collateral"
          , "10000"
          , -- Required signer
            "--required-signer-hash"
          , "f2998eb67942c4674d01e2cd435e1f17919e095eec43807bb0010313"
          , -- Certificate
            "--certificate-file"
          , inputDir </> "stake-address-registration.json"
          , "--certificate-script-file"
          , inputDir </> "AlwaysSucceeds.plutus"
          , "--certificate-redeemer-value"
          , "63"
          , "--certificate-execution-units"
          , "(100,110)"
          , -- Withdrawal
            "--withdrawal"
          , "stake_test17qvxuvh64q9zdqgrjt76d42eclk5wgdxtnsun4808cwg0dqxv5r99+10000"
          , "--withdrawal-plutus-script-v3"
          , "--withdrawal-reference-tx-in-redeemer-value"
          , "83"
          , "--withdrawal-reference-tx-in-execution-units"
          , "(100,110)"
          , "--withdrawal-tx-in-reference"
          , "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891#141"
          , -- Auxiliary script file
            "--auxiliary-script-file"
          , inputDir </> "AlwaysSucceeds.plutus"
          , -- Metadata
            "--metadata-json-file"
          , inputDir </> "tx_metadata_noschema.json"
          , -- Proposal
            "--proposal-file"
          , inputDir </> "conway" </> "conway-create-protocol-parameters-update.action"
          , -- Vote
            "--vote-file"
          , inputDir </> "conway" </> "vote1.drep.json"
          , -- Treasury donation
            "--treasury-donation"
          , "1000000"
          , "--current-treasury-value"
          , "1000000000"
          , -- Tx Outputs
            "--tx-out"
          , mconcat
              [ "addr_test1"
              , "qrefnr4k09pvge6dq83v6s67ruter8sftmky8qrmkqqsxy7q5psgn8tgqmupq4r7"
              , "9jmxlyk4eqt6z6hj5g8jd8393msqaw47f4"
              , " + "
              , "138"
              , " + "
              , "130 d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf"
              , " + "
              , "132 a06ee5ffdd7f9b5bd992eb9543f44418323f81229526b77b0e4be067.cafe"
              , " + "
              , "134 d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf.f00d"
              , " + "
              , "136 a06ee5ffdd7f9b5bd992eb9543f44418323f81229526b77b0e4be067.dead"
              , " + "
              , "138"
              , " d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf"
              , ".736e6f77"
              , " + "
              , "142"
              , " a06ee5ffdd7f9b5bd992eb9543f44418323f81229526b77b0e4be067"
              , ".736b79"
              ]
          , "--fee"
          , "139"
          , "--invalid-before"
          , "140"
          , "--mint"
          , mconcat
              [ "130 d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf"
              , " + "
              , "132 a06ee5ffdd7f9b5bd992eb9543f44418323f81229526b77b0e4be067.cafe"
              , " + "
              , "134 d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf.f00d"
              , " + "
              , "136 a06ee5ffdd7f9b5bd992eb9543f44418323f81229526b77b0e4be067.dead"
              , " + "
              , "138"
              , " d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf"
              , ".736e6f77"
              , " + "
              , "142"
              , " a06ee5ffdd7f9b5bd992eb9543f44418323f81229526b77b0e4be067"
              , ".736b79"
              ]
          , "--mint-script-file"
          , inputDir </> "alonzo/scripts/mint.all"
          , "--mint-script-file"
          , inputDir </> "alonzo/scripts/mint.sig"
          , "--out-file"
          , transactionBodyFile
          ]

      -- View transaction body
      result <-
        execCardanoCLI
          ["debug", "transaction", "view", "--tx-body-file", transactionBodyFile, "--output-yaml"]
      H.diffVsGoldenFile result $ goldenDir </> "alonzo/transaction-view.out"

-- | Test metadata format
hprop_golden_view_metadata :: Property
hprop_golden_view_metadata =
  watchdogProp . propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
    transactionBodyMetaNoSchema <- noteTempFile tempDir "transaction-body-noschema"
    makeTxBody TxMetadataJsonNoSchema transactionBodyMetaNoSchema
    resultNoSchema <-
      execCardanoCLI
        ["debug", "transaction", "view", "--tx-body-file", transactionBodyMetaNoSchema, "--output-yaml"]
    H.diffVsGoldenFile resultNoSchema $ goldenDir </> "conway/transaction-view-metadata-noschema.out"

    transactionBodyMetaDetailedSchema <- noteTempFile tempDir "transaction-body-detailedschema"
    makeTxBody TxMetadataJsonDetailedSchema transactionBodyMetaDetailedSchema
    resultDetailedSchema <-
      execCardanoCLI
        [ "debug"
        , "transaction"
        , "view"
        , "--tx-body-file"
        , transactionBodyMetaDetailedSchema
        , "--output-yaml"
        ]
    H.diffVsGoldenFile resultDetailedSchema $
      goldenDir </> "conway/transaction-view-metadata-detailedschema.out"
 where
  makeTxBody :: TxMetadataJsonSchema -> FilePath -> Integration ()
  makeTxBody hasSchema transactionBodyFile = do
    let metadataArgs =
          case hasSchema of
            TxMetadataJsonNoSchema ->
              ["--metadata-json-file", inputDir </> "tx_metadata_noschema.json"]
            TxMetadataJsonDetailedSchema ->
              [ "--json-metadata-detailed-schema"
              , "--metadata-json-file"
              , inputDir </> "tx_metadata_withschema.json"
              ]
    void . execCardanoCLI $
      [ "conway"
      , "transaction"
      , "build-raw"
      , "--tx-in"
      , "ed7c8f68c194cc763ee65ad22ef0973e26481be058c65005fd39fb93f9c43a20#213"
      , "--tx-out"
      , "addr_test1vpfwv0ezc5g8a4mkku8hhy3y3vp92t7s3ul8g778g5yegsgalc6gc+24910487859"
      , "--fee"
      , "21300"
      , "--out-file"
      , transactionBodyFile
      ]
        <> metadataArgs

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden view conway proposal/"'@
hprop_golden_view_conway_proposal :: Property
hprop_golden_view_conway_proposal =
  watchdogProp . propertyOnce $ do
    let golden = goldenDir </> "conway"
        input = inputDir </> "conway"

    result <-
      execCardanoCLI
        ["debug", "transaction", "view", "--tx-file", input </> "tx-proposal.json"]

    H.diffVsGoldenFile result (golden </> "tx-proposal.out.json")
