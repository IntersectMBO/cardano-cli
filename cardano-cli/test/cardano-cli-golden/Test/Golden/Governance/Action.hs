{-# LANGUAGE FlexibleContexts #-}
{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Golden.Governance.Action where

import           Cardano.Api (MonadIO)

import           Control.Monad (void)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Trans.Control (MonadBaseControl)

import           Test.Cardano.CLI.Hash (exampleAnchorDataHash, exampleAnchorDataHash2,
                   exampleAnchorDataIpfsHash, exampleAnchorDataIpfsHash2,
                   exampleAnchorDataPathGolden, exampleAnchorDataPathGolden2, serveFilesWhile)
import qualified Test.Cardano.CLI.Util as H
import           Test.Cardano.CLI.Util (execCardanoCLI, execCardanoCLIWithEnvVars, expectFailure,
                   noteInputFile, noteTempFile, propertyOnce)

import           Hedgehog (MonadTest, Property)
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Test.Golden as H

hprop_golden_governance_action_create_constitution_wrong_hash1_fails :: Property
hprop_golden_governance_action_create_constitution_wrong_hash1_fails =
  propertyOnce . expectFailure . H.moduleWorkspace "tmp" $ \tempDir ->
    base_golden_governance_action_create_constitution
      ('a' : drop 1 exampleAnchorDataHash)
      exampleAnchorDataHash2
      tempDir

hprop_golden_governance_action_create_constitution_wrong_hash2_fails :: Property
hprop_golden_governance_action_create_constitution_wrong_hash2_fails =
  propertyOnce . expectFailure . H.moduleWorkspace "tmp" $ \tempDir ->
    base_golden_governance_action_create_constitution
      exampleAnchorDataHash
      ('a' : drop 1 exampleAnchorDataHash2)
      tempDir

hprop_golden_governance_action_create_constitution :: Property
hprop_golden_governance_action_create_constitution =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir ->
    base_golden_governance_action_create_constitution
      exampleAnchorDataHash
      exampleAnchorDataHash2
      tempDir

    void $
      execCardanoCLI
        [ "latest"
        , "stake-address"
        , "key-gen"
        , "--verification-key-file"
        , stakeAddressVKeyFile
        , "--signing-key-file"
        , stakeAddressSKeyFile
        ]

  void $
    execCardanoCLI
      [ "legacy"
      , "stake-address"
      , "key-gen"
      , "--verification-key-file"
      , stakeAddressVKeyFile
      , "--signing-key-file"
      , stakeAddressSKeyFile
      ]

  actionFile <- noteTempFile tempDir "create-constitution.action"
  redactedActionFile <- noteTempFile tempDir "create-constitution.action.redacted"

  let relativeUrl1 = ["ipfs", exampleAnchorDataIpfsHash]
  let relativeUrl2 = ["ipfs", exampleAnchorDataIpfsHash2]

  serveFilesWhile
    [ (relativeUrl1, exampleAnchorDataPathGolden)
    , (relativeUrl2, exampleAnchorDataPathGolden2)
    ]
    ( \port -> do
        void $
          execCardanoCLIWithEnvVars
            [("IPFS_GATEWAY_URI", "http://localhost:" ++ show port ++ "/")]
            [ "conway"
            , "governance"
            , "action"
            , "create-constitution"
            , "--mainnet"
            , "--anchor-data-hash"
            , hash1
            , "--anchor-url"
            , "ipfs://" ++ exampleAnchorDataIpfsHash
            , "--check-anchor-data"
            , "--governance-action-deposit"
            , "10"
            , "--deposit-return-stake-verification-key-file"
            , stakeAddressVKeyFile
            , "--out-file"
            , actionFile
            , "--constitution-url"
            , "ipfs://" ++ exampleAnchorDataIpfsHash2
            , "--constitution-hash"
            , hash2
            , "--check-constitution-hash"
            ]
    )

  goldenActionFile <-
    H.note
      "test/cardano-cli-golden/files/golden/governance/action/create-constitution-for-stake-address.action.golden"

  -- Remove cbor hex from comparison, as it's not stable
  H.redactJsonField "cborHex" "<cborHex>" actionFile redactedActionFile

  H.diffFileVsGoldenFile redactedActionFile goldenActionFile

hprop_golden_conway_governance_action_view_constitution_json :: Property
hprop_golden_conway_governance_action_view_constitution_json =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    stakeAddressVKeyFile <- H.note "test/cardano-cli-golden/files/input/governance/stake-address.vkey"
    hashFile <- noteTempFile tempDir "hash.txt"

    actionFile <- noteTempFile tempDir "action"

    -- We go through a file for the hash, to test --out-file
    void $
      execCardanoCLI
        [ "hash"
        , "anchor-data"
        , "--text"
        , "whatever "
        , "--out-file"
        , hashFile
        ]

    proposalHash <- H.readFile hashFile

    constitutionHash <-
      execCardanoCLI
        [ "hash"
        , "anchor-data"
        , "--text"
        , "nonAsciiInput: 你好 and some more: こんにちは"
        ]

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "action"
        , "create-constitution"
        , "--mainnet"
        , "--anchor-data-hash"
        , proposalHash
        , "--anchor-url"
        , "proposal-dummy-url"
        , "--trust-anchor-data"
        , "--governance-action-deposit"
        , "10"
        , "--deposit-return-stake-verification-key-file"
        , stakeAddressVKeyFile
        , "--out-file"
        , actionFile
        , "--constitution-url"
        , "http://my-great-constitution.rocks"
        , "--constitution-hash"
        , constitutionHash
        , "--trust-constitution-hash"
        ]

    goldenActionViewFile <-
      H.note "test/cardano-cli-golden/files/golden/governance/action/view/create-constitution.action.view"
    actionView <-
      execCardanoCLI
        [ "conway"
        , "governance"
        , "action"
        , "view"
        , "--action-file"
        , actionFile
        ]
    H.diffVsGoldenFile actionView goldenActionViewFile

hprop_golden_conway_governance_action_view_update_committee_yaml_wrong_hash_fails :: Property
hprop_golden_conway_governance_action_view_update_committee_yaml_wrong_hash_fails =
  propertyOnce . expectFailure . H.moduleWorkspace "tmp" $ \tempDir ->
    base_golden_conway_governance_action_view_update_committee_yaml
      ('a' : drop 1 exampleAnchorDataHash)
      tempDir

hprop_golden_conway_governance_action_view_update_committee_yaml :: Property
hprop_golden_conway_governance_action_view_update_committee_yaml =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir ->
    base_golden_conway_governance_action_view_update_committee_yaml exampleAnchorDataHash tempDir

base_golden_conway_governance_action_view_update_committee_yaml
  :: (MonadBaseControl IO m, MonadTest m, MonadIO m, MonadCatch m) => String -> FilePath -> m ()
base_golden_conway_governance_action_view_update_committee_yaml hash tempDir = do
  stakeAddressVKeyFile <- H.note "test/cardano-cli-golden/files/input/governance/stake-address.vkey"

  actionFile <- noteTempFile tempDir "action"

  let relativeUrl = ["ipfs", exampleAnchorDataIpfsHash]

  serveFilesWhile
    [(relativeUrl, exampleAnchorDataPathGolden)]
    ( \port -> do
        void $
          execCardanoCLIWithEnvVars
            [("IPFS_GATEWAY_URI", "http://localhost:" ++ show port ++ "/")]
            [ "conway"
            , "governance"
            , "action"
            , "update-committee"
            , "--mainnet"
            , "--governance-action-deposit"
            , "10"
            , "--deposit-return-stake-verification-key-file"
            , stakeAddressVKeyFile
            , "--anchor-url"
            , "ipfs://" ++ exampleAnchorDataIpfsHash
            , "--anchor-data-hash"
            , hash
            , "--check-anchor-data"
            , "--threshold"
            , "0.61"
            , "--out-file"
            , actionFile
            ]
    )

  goldenActionViewFile <-
    H.note "test/cardano-cli-golden/files/golden/governance/action/view/update-committee.action.view"
  actionView <-
    execCardanoCLI
      [ "conway"
      , "governance"
      , "action"
      , "view"
      , "--action-file"
      , actionFile
      , "--output-yaml"
      ]
  H.diffVsGoldenFile actionView goldenActionViewFile

hprop_golden_conway_governance_action_view_create_info_json_outfile_wrong_hash_fails :: Property
hprop_golden_conway_governance_action_view_create_info_json_outfile_wrong_hash_fails =
  propertyOnce . expectFailure . H.moduleWorkspace "tmp" $ \tempDir ->
    base_golden_conway_governance_action_view_create_info_json_outfile
      ('a' : drop 1 exampleAnchorDataHash)
      tempDir

hprop_golden_conway_governance_action_view_create_info_json_outfile :: Property
hprop_golden_conway_governance_action_view_create_info_json_outfile =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir ->
    base_golden_conway_governance_action_view_create_info_json_outfile exampleAnchorDataHash tempDir

base_golden_conway_governance_action_view_create_info_json_outfile
  :: (MonadBaseControl IO m, MonadTest m, MonadIO m, MonadCatch m) => String -> FilePath -> m ()
base_golden_conway_governance_action_view_create_info_json_outfile hash tempDir = do
  stakeAddressVKeyFile <- H.note "test/cardano-cli-golden/files/input/governance/stake-address.vkey"

  actionFile <- noteTempFile tempDir "action"

  let relativeUrl = ["ipfs", exampleAnchorDataIpfsHash]
  serveFilesWhile
    [(relativeUrl, exampleAnchorDataPathGolden)]
    ( \port -> do
        void $
          execCardanoCLIWithEnvVars
            [("IPFS_GATEWAY_URI", "http://localhost:" ++ show port ++ "/")]
            [ "conway"
            , "governance"
            , "action"
            , "create-info"
            , "--testnet"
            , "--governance-action-deposit"
            , "10"
            , "--deposit-return-stake-verification-key-file"
            , stakeAddressVKeyFile
            , "--anchor-url"
            , "ipfs://" ++ exampleAnchorDataIpfsHash
            , "--anchor-data-hash"
            , hash
            , "--check-anchor-data"
            , "--out-file"
            , actionFile
            ]
    )

  actionViewFile <- noteTempFile tempDir "action-view"
  goldenActionViewFile <-
    H.note "test/cardano-cli-golden/files/golden/governance/action/view/create-info.action.view"
  void $
    execCardanoCLI
      [ "conway"
      , "governance"
      , "action"
      , "view"
      , "--action-file"
      , actionFile
      , "--out-file"
      , actionViewFile
      ]
  H.diffFileVsGoldenFile actionViewFile goldenActionViewFile

hprop_golden_governanceActionCreateNoConfidence_wrong_hash_fails :: Property
hprop_golden_governanceActionCreateNoConfidence_wrong_hash_fails =
  propertyOnce . expectFailure . H.moduleWorkspace "tmp" $ \tempDir ->
    base_golden_governanceActionCreateNoConfidence
      ('a' : drop 1 exampleAnchorDataHash)
      tempDir

hprop_golden_governanceActionCreateNoConfidence :: Property
hprop_golden_governanceActionCreateNoConfidence =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir ->
    base_golden_governanceActionCreateNoConfidence exampleAnchorDataHash tempDir

base_golden_governanceActionCreateNoConfidence
  :: (MonadBaseControl IO m, MonadTest m, MonadIO m, MonadCatch m) => String -> FilePath -> m ()
base_golden_governanceActionCreateNoConfidence hash tempDir = do
  stakeAddressVKeyFile <-
    noteInputFile "test/cardano-cli-golden/files/input/governance/stake-address.vkey"

  actionFile <- noteTempFile tempDir "action"

  let relativeUrl = ["ipfs", exampleAnchorDataIpfsHash]
  serveFilesWhile
    [(relativeUrl, exampleAnchorDataPathGolden)]
    ( \port -> do
        void $
          execCardanoCLIWithEnvVars
            [("IPFS_GATEWAY_URI", "http://localhost:" ++ show port ++ "/")]
            [ "conway"
            , "governance"
            , "action"
            , "create-no-confidence"
            , "--mainnet"
            , "--governance-action-deposit"
            , "10"
            , "--deposit-return-stake-verification-key-file"
            , stakeAddressVKeyFile
            , "--anchor-url"
            , "ipfs://" ++ exampleAnchorDataIpfsHash
            , "--anchor-data-hash"
            , hash
            , "--check-anchor-data"
            , "--prev-governance-action-index"
            , "5"
            , "--prev-governance-action-tx-id"
            , "b1015258a99351c143a7a40b7b58f033ace10e3cc09c67780ed5b2b0992aa60a"
            , "--out-file"
            , actionFile
            ]
    )

  actionViewFile <- noteTempFile tempDir "action-view"
  goldenActionViewFile <-
    H.note
      "test/cardano-cli-golden/files/golden/governance/action/view/create-no-confidence.action.view"
  void $
    execCardanoCLI
      [ "conway"
      , "governance"
      , "action"
      , "view"
      , "--action-file"
      , actionFile
      , "--out-file"
      , actionViewFile
      ]
  H.diffFileVsGoldenFile actionViewFile goldenActionViewFile

hprop_golden_conway_governance_action_create_protocol_parameters_update_wrong_hash_fails :: Property
hprop_golden_conway_governance_action_create_protocol_parameters_update_wrong_hash_fails =
  propertyOnce . expectFailure . H.moduleWorkspace "tmp" $ \tempDir ->
    base_golden_conway_governance_action_create_protocol_parameters_update
      ('a' : drop 1 exampleAnchorDataHash)
      tempDir

hprop_golden_conway_governance_action_create_protocol_parameters_update :: Property
hprop_golden_conway_governance_action_create_protocol_parameters_update =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir ->
    base_golden_conway_governance_action_create_protocol_parameters_update exampleAnchorDataHash tempDir

base_golden_conway_governance_action_create_protocol_parameters_update
  :: (MonadBaseControl IO m, MonadTest m, MonadIO m, MonadCatch m) => String -> FilePath -> m ()
base_golden_conway_governance_action_create_protocol_parameters_update hash tempDir = do
  stakeAddressVKeyFile <- H.note "test/cardano-cli-golden/files/input/governance/stake-address.vkey"
  -- different versions of https://github.com/IntersectMBO/plutus/blob/master/plutus-core/cost-model/data/builtinCostModel.json
  -- transformed and compiled together
  costModelsFile <- H.note "test/cardano-cli-golden/files/input/governance/costmodels.json"

  actionFile <- noteTempFile tempDir "action"

  let relativeUrl = ["ipfs", exampleAnchorDataIpfsHash]
  serveFilesWhile
    [(relativeUrl, exampleAnchorDataPathGolden)]
    ( \port -> do
        void $
          execCardanoCLIWithEnvVars
            [("IPFS_GATEWAY_URI", "http://localhost:" ++ show port ++ "/")]
            [ "conway"
            , "governance"
            , "action"
            , "create-protocol-parameters-update"
            , "--anchor-url"
            , "ipfs://" ++ exampleAnchorDataIpfsHash
            , "--anchor-data-hash"
            , hash
            , "--check-anchor-data"
            , "--mainnet"
            , "--deposit-return-stake-verification-key-file"
            , stakeAddressVKeyFile
            , "--governance-action-deposit"
            , "12345"
            , "--new-governance-action-deposit"
            , "123454321"
            , "--max-tx-size"
            , "1234"
            , "--cost-model-file"
            , costModelsFile
            , "--out-file"
            , actionFile
            ]
    )

  goldenActionFile <-
    H.note
      "test/cardano-cli-golden/files/golden/governance/action/conway-create-protocol-parameters-update.action"
  H.diffFileVsGoldenFile actionFile goldenActionFile

hprop_golden_conway_governance_action_create_protocol_parameters_update_partial_costmodel
  :: Property
hprop_golden_conway_governance_action_create_protocol_parameters_update_partial_costmodel =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    stakeAddressVKeyFile <- H.note "test/cardano-cli-golden/files/input/governance/stake-address.vkey"
    costModelsFile <- H.note "test/cardano-cli-golden/files/input/governance/costmodels-partial.json"

    actionFile <- noteTempFile tempDir "action"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "action"
        , "create-protocol-parameters-update"
        , "--anchor-url"
        , "example.com"
        , "--anchor-data-hash"
        , "c7ddb5b493faa4d3d2d679847740bdce0c5d358d56f9b1470ca67f5652a02745"
        , "--mainnet"
        , "--trust-anchor-data"
        , "--deposit-return-stake-verification-key-file"
        , stakeAddressVKeyFile
        , "--governance-action-deposit"
        , "12345"
        , "--cost-model-file"
        , costModelsFile
        , "--out-file"
        , actionFile
        ]

    goldenActionFile <-
      H.note
        "test/cardano-cli-golden/files/golden/governance/action/conway-create-protocol-parameters-update-partial-costmodels.action"
    H.diffFileVsGoldenFile actionFile goldenActionFile

hprop_golden_conway_governance_action_create_hardfork_wrong_hash_fails :: Property
hprop_golden_conway_governance_action_create_hardfork_wrong_hash_fails =
  propertyOnce . expectFailure . H.moduleWorkspace "tmp" $ \tempDir ->
    base_golden_conway_governance_action_create_hardfork
      ('a' : drop 1 exampleAnchorDataHash)
      tempDir

hprop_golden_conway_governance_action_create_hardfork :: Property
hprop_golden_conway_governance_action_create_hardfork =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir ->
    base_golden_conway_governance_action_create_hardfork exampleAnchorDataHash tempDir

base_golden_conway_governance_action_create_hardfork
  :: (MonadBaseControl IO m, MonadTest m, MonadIO m, MonadCatch m) => String -> FilePath -> m ()
base_golden_conway_governance_action_create_hardfork hash tempDir = do
  stakeAddressVKeyFile <- H.note "test/cardano-cli-golden/files/input/governance/stake-address.vkey"

  actionFile <- noteTempFile tempDir "action"

  let relativeUrl = ["ipfs", exampleAnchorDataIpfsHash]
  serveFilesWhile
    [(relativeUrl, exampleAnchorDataPathGolden)]
    ( \port -> do
        void $
          execCardanoCLIWithEnvVars
            [("IPFS_GATEWAY_URI", "http://localhost:" ++ show port ++ "/")]
            [ "conway"
            , "governance"
            , "action"
            , "create-hardfork"
            , "--anchor-url"
            , "ipfs://" ++ exampleAnchorDataIpfsHash
            , "--anchor-data-hash"
            , hash
            , "--check-anchor-data"
            , "--mainnet"
            , "--deposit-return-stake-verification-key-file"
            , stakeAddressVKeyFile
            , "--governance-action-deposit"
            , "12345"
            , "--protocol-major-version"
            , "10"
            , "--protocol-minor-version"
            , "0"
            , "--out-file"
            , actionFile
            ]
    )

  goldenActionFile <-
    H.note
      "test/cardano-cli-golden/files/golden/governance/action/hardfork/conway-create-hardfork.action"
  H.diffFileVsGoldenFile actionFile goldenActionFile
