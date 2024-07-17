{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.TxView where

import           Cardano.Api (TxMetadataJsonSchema (..))

import           Control.Monad (void)
import           System.FilePath ((</>))

import           Test.Cardano.CLI.Util (execCardanoCLI, noteTempFile)

import           Hedgehog (Property)
import           Hedgehog.Extras (Integration, moduleWorkspace, note_, propertyOnce)
import qualified Hedgehog.Extras.Test.Golden as H

goldenDir, inputDir :: FilePath
goldenDir = "test/cardano-cli-golden/files/golden"
inputDir = "test/cardano-cli-golden/files/input"

hprop_golden_view_shelley_yaml :: Property
hprop_golden_view_shelley_yaml =
  let
    certDir = inputDir </> "shelley/certificates"
    certs =
      (certDir </>)
        <$> [ "genesis_key_delegation_certificate"
            , "mir_certificate"
            , "stake_address_deregistration_certificate"
            , "stake_address_registration_certificate"
            , "stake_pool_deregistration_certificate"
            , "stake_pool_registration_certificate"
            ]
   in
    propertyOnce $
      moduleWorkspace "tmp" $ \tempDir -> do
        updateProposalFile <- noteTempFile tempDir "update-proposal"
        transactionBodyFile <- noteTempFile tempDir "transaction-body"

        let extraEntropySeed = "c0ffee"
        note_ $ "extra entropy seed: " ++ extraEntropySeed
        note_ $
          mconcat
            [ "extra entropy hash:"
            , " 88f04f011dcded879039ae4b9b20219d9448e5c7b42c2d1f638fb8740e0ab8be"
            ]

        note_ $
          mconcat
            [ "genesis-verification-key-file hash:"
            , " 81cb0bc5b6fbba391e6f7ec3d9271cbea25bcbf907181b7c4d5f8c2f"
            ]

        -- Create update proposal
        void $
          execCardanoCLI
            [ "legacy"
            , "governance"
            , "create-update-proposal"
            , "--decentralization-parameter"
            , "63/64"
            , "--epoch"
            , "64"
            , "--extra-entropy"
            , extraEntropySeed
            , "--genesis-verification-key-file"
            , inputDir </> "shelley/keys/genesis_keys/verification_key"
            , "--key-reg-deposit-amt"
            , "71"
            , "--max-block-body-size"
            , "72"
            , "--max-block-header-size"
            , "73"
            , "--max-tx-size"
            , "74"
            , "--min-fee-constant"
            , "75"
            , "--min-fee-linear"
            , "76"
            , "--min-pool-cost"
            , "77"
            , "--min-utxo-value"
            , "78"
            , "--monetary-expansion"
            , "79/80"
            , "--number-of-pools"
            , "80"
            , "--out-file"
            , updateProposalFile
            , "--pool-influence"
            , "82/83"
            , "--pool-reg-deposit"
            , "83"
            , "--pool-retirement-epoch-boundary"
            , "84"
            , "--protocol-major-version"
            , "8"
            , "--protocol-minor-version"
            , "86"
            , "--treasury-expansion"
            , "87/88"
            ]

        -- Create transaction body
        void $
          execCardanoCLI $
            [ "shelley"
            , "transaction"
            , "build-raw"
            , "--tx-in"
            , "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891#29"
            , "--tx-out"
            , "addr_test1vz7w0r9epak6nmnh3mc8e2ypkjyu8zsc3xf7dpct6k577acxmcfyv+31"
            , "--fee"
            , "32"
            , "--invalid-hereafter"
            , "33"
            , "--withdrawal"
            , "stake_test1up00fz9lyqs5sjks82k22eqz7a9srym9vysjgp3h2ua2v2cm522kg+42"
            , "--update-proposal-file"
            , updateProposalFile
            , "--out-file"
            , transactionBodyFile
            ]
              ++ ["--certificate-file=" <> cert | cert <- certs]

        -- View transaction body
        result <-
          execCardanoCLI
            ["debug", "transaction", "view", "--tx-body-file", transactionBodyFile, "--output-yaml"]

        H.diffVsGoldenFile result $ goldenDir </> "shelley/transaction-view.out"

hprop_golden_view_allegra_yaml :: Property
hprop_golden_view_allegra_yaml =
  propertyOnce $
    moduleWorkspace "tmp" $ \tempDir -> do
      transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

      -- Create transaction body
      void $
        execCardanoCLI
          [ "allegra"
          , "transaction"
          , "build-raw"
          , "--tx-in"
          , "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891#94"
          , "--tx-out"
          , mconcat
              [ "addr_test1"
              , "qrefnr4k09pvge6dq83v6s67ruter8sftmky8qrmkqqsxy7q5psgn8tgqmupq4r7"
              , "9jmxlyk4eqt6z6hj5g8jd8393msqaw47f4"
              , "+99"
              ]
          , "--fee"
          , "100"
          , "--invalid-hereafter"
          , "101"
          , "--out-file"
          , transactionBodyFile
          ]

      -- View transaction body
      result <-
        execCardanoCLI
          ["debug", "transaction", "view", "--tx-body-file", transactionBodyFile, "--output-yaml"]
      H.diffVsGoldenFile result $ goldenDir </> "allegra/transaction-view.out"

hprop_golden_view_mary_yaml :: Property
hprop_golden_view_mary_yaml =
  propertyOnce $
    moduleWorkspace "tmp" $ \tempDir -> do
      transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

      -- Create transaction body
      void $
        execCardanoCLI
          [ "mary"
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
          , inputDir </> "mary/scripts/mint.all"
          , "--mint-script-file"
          , inputDir </> "mary/scripts/mint.sig"
          , "--out-file"
          , transactionBodyFile
          ]

      -- View transaction body
      result <-
        execCardanoCLI
          ["debug", "transaction", "view", "--tx-body-file", transactionBodyFile, "--output-yaml"]
      H.diffVsGoldenFile result $ goldenDir </> "mary/transaction-view.out"

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

createAlonzoTxBody :: Maybe FilePath -> FilePath -> Integration ()
createAlonzoTxBody mUpdateProposalFile transactionBodyFile = do
  void $
    execCardanoCLI
      ( [ "alonzo"
        , "transaction"
        , "build-raw"
        , "--tx-in"
        , "ed7c8f68c194cc763ee65ad22ef0973e26481be058c65005fd39fb93f9c43a20#212"
        , "--tx-in-collateral"
        , "c9765d7d0e3955be8920e6d7a38e1f3f2032eac48c7c59b0b9193caa87727e7e#256"
        , "--fee"
        , "213"
        , "--required-signer-hash"
        , "98717eaba8105a50a2a71831267552e337dfdc893bef5e40b8676d27"
        , "--required-signer-hash"
        , "fafaaac8681b5050a8987f95bce4a7f99362f189879258fdbf733fa4"
        , "--out-file"
        , transactionBodyFile
        ]
          ++ [ "--update-proposal-file=" <> updateProposalFile
             | Just updateProposalFile <- [mUpdateProposalFile]
             ]
      )

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden view alonzo yaml/"'@
hprop_golden_view_alonzo_yaml :: Property
hprop_golden_view_alonzo_yaml =
  propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
    updateProposalFile <- noteTempFile tempDir "update-proposal"
    transactionBodyFile <- noteTempFile tempDir "transaction-body"

    note_ $
      mconcat
        [ "genesis-verification-key-file hash:"
        , " 1bafa294233a5a7ffbf539ae798da0943aa83d2a19398c2d0e5af114"
        ]

    -- Create update proposal
    void $
      execCardanoCLI
        [ "legacy"
        , "governance"
        , "create-update-proposal"
        , "--epoch"
        , "190"
        , "--genesis-verification-key-file"
        , inputDir </> "shelley/keys/genesis_keys/verification_key"
        , "--price-execution-steps"
        , "195/196"
        , "--price-execution-memory"
        , "196/197"
        , "--max-tx-execution-units"
        , "(197, 198)"
        , "--max-block-execution-units"
        , "(198, 199)"
        , "--max-value-size"
        , "199"
        , "--collateral-percent"
        , "200"
        , "--max-collateral-inputs"
        , "201"
        , "--out-file"
        , updateProposalFile
        ]

    createAlonzoTxBody (Just updateProposalFile) transactionBodyFile

    -- View transaction body
    result <-
      execCardanoCLI
        ["debug", "transaction", "view", "--tx-body-file", transactionBodyFile, "--output-yaml"]
    H.diffVsGoldenFile result $ goldenDir </> "alonzo/transaction-view.out"

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden view alonzo signed yaml/"'@
hprop_golden_view_alonzo_signed_yaml :: Property
hprop_golden_view_alonzo_signed_yaml =
  propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
    let golden = goldenDir </> "alonzo"
        input = inputDir </> "alonzo"

    transactionBodyFile <- noteTempFile tempDir "transaction-body"
    transactionFile <- noteTempFile tempDir "transaction"

    createAlonzoTxBody Nothing transactionBodyFile

    -- Sign
    void $
      execCardanoCLI
        [ "transaction"
        , "sign"
        , "--tx-body-file"
        , transactionBodyFile
        , "--signing-key-file"
        , input </> "signing.key"
        , "--out-file"
        , transactionFile
        ]

    -- View transaction body
    result <-
      execCardanoCLI
        ["debug", "transaction", "view", "--tx-file", transactionFile, "--output-yaml"]

    H.diffVsGoldenFile result (golden </> "signed-transaction-view.out")

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden view conway three votes/"'@
hprop_golden_view_conway_three_votes :: Property
hprop_golden_view_conway_three_votes =
  propertyOnce $ do
    let golden = goldenDir </> "conway"
        input = inputDir </> "conway"

    result <-
      execCardanoCLI
        ["debug", "transaction", "view", "--tx-file", input </> "tx-three-votes.json", "--output-json"]

    H.diffVsGoldenFile result (golden </> "tx-three-votes-view.out.json")

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
