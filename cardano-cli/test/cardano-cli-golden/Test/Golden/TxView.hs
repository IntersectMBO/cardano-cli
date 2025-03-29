{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.TxView where

import Cardano.Api (TxMetadataJsonSchema (..))

import Control.Monad (void)
import System.FilePath ((</>))

import Test.Cardano.CLI.Util (execCardanoCLI, noteTempFile)

import Hedgehog (Property)
import Hedgehog.Extras (Integration, moduleWorkspace, propertyOnce)
import Hedgehog.Extras.Test.Golden qualified as H

goldenDir, inputDir :: FilePath
goldenDir = "test/cardano-cli-golden/files/golden"
inputDir = "test/cardano-cli-golden/files/input"

hprop_golden_view_alonzo_yaml :: Property
hprop_golden_view_alonzo_yaml =
  propertyOnce $
    moduleWorkspace "tmp" $ \tempDir -> do
      transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

      -- Create transaction body
      void $
        execCardanoCLI
          [ "alonzo"
          , "transaction"
          , "build-raw"
          , "--tx-in"
          , "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891#135"
          , "--tx-out"
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

hprop_golden_view_redeemer :: Property
hprop_golden_view_redeemer = do
  propertyOnce $
    moduleWorkspace "tmp" $ \tempDir -> do
      transactionBodyFile <- noteTempFile tempDir "transaction-body-file"
      scriptTxBody transactionBodyFile

      -- View transaction body
      result <-
        execCardanoCLI
          ["debug", "transaction", "view", "--tx-body-file", transactionBodyFile, "--output-yaml"]

      H.diffVsGoldenFile result $ goldenDir </> "babbage/transaction-view-redeemer.out"
 where
  scriptTxBody :: FilePath -> Integration ()
  scriptTxBody transactionBodyFile =
    void $
      execCardanoCLI
        [ "babbage"
        , "transaction"
        , "build-raw"
        , "--tx-in"
        , "ed7c8f68c194cc763ee65ad22ef0973e26481be058c65005fd39fb93f9c43a20#213"
        , "--tx-in-datum-value"
        , "6666"
        , "--tx-in-redeemer-value"
        , "42"
        , "--tx-in-script-file"
        , inputDir </> "AlwaysSucceeds.plutus"
        , "--tx-in-execution-units"
        , "(100, 200)"
        , "--tx-in-collateral"
        , "c9765d7d0e3955be8920e6d7a38e1f3f2032eac48c7c59b0b9193caa87727e7e#256"
        , "--protocol-params-file"
        , inputDir </> "babbage/transaction-calculate-min-fee/protocol-params.json"
        , "--fee"
        , "213"
        , "--out-file"
        , transactionBodyFile
        ]

-- | Test metadata format
hprop_golden_view_metadata :: Property
hprop_golden_view_metadata = propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
  transactionBodyMetaNoSchema <- noteTempFile tempDir "transaction-body-noschema"
  makeTxBody TxMetadataJsonNoSchema transactionBodyMetaNoSchema
  resultNoSchema <-
    execCardanoCLI
      ["debug", "transaction", "view", "--tx-body-file", transactionBodyMetaNoSchema, "--output-yaml"]
  H.diffVsGoldenFile resultNoSchema $ goldenDir </> "babbage/transaction-view-metadata-noschema.out"

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
    goldenDir </> "babbage/transaction-view-metadata-detailedschema.out"
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
      [ "babbage"
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
  propertyOnce $ do
    let golden = goldenDir </> "conway"
        input = inputDir </> "conway"

    result <-
      execCardanoCLI
        ["debug", "transaction", "view", "--tx-file", input </> "tx-proposal.json", "--output-json"]

    H.diffVsGoldenFile result (golden </> "tx-proposal.out.json")
