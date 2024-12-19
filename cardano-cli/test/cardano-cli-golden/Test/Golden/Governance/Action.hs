{-# LANGUAGE FlexibleContexts #-}
{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Golden.Governance.Action where

import           Cardano.Api (MonadIO)

import           Control.Monad (void)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Trans.Control (MonadBaseControl)

import           Test.Cardano.CLI.Hash (exampleGovActionAnchorHash1, exampleGovActionAnchorHash2,
                   exampleGovActionAnchorIpfsHash1, exampleGovActionAnchorIpfsHash2,
                   exampleGovActionAnchorPathGolden1, exampleGovActionAnchorPathGolden2,
                   serveFilesWhile, tamperBase16Hash)
import qualified Test.Cardano.CLI.Util as H
import           Test.Cardano.CLI.Util (execCardanoCLI, execCardanoCLIWithEnvVars, expectFailure,
                   noteInputFile, noteTempFile, propertyOnce)

import           Hedgehog (MonadTest, Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Test.Golden as H

hprop_golden_governance_action_create_constitution_wrong_hash1_fails :: Property
hprop_golden_governance_action_create_constitution_wrong_hash1_fails =
  propertyOnce . expectFailure . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- We modify the hash slightly so that the hash check fails
    alteredHash <- H.evalMaybe $ tamperBase16Hash exampleGovActionAnchorHash1
    -- We run the test with the modified hash
    base_golden_governance_action_create_constitution
      alteredHash
      exampleGovActionAnchorHash2
      tempDir

hprop_golden_governance_action_create_constitution_wrong_hash2_fails :: Property
hprop_golden_governance_action_create_constitution_wrong_hash2_fails =
  propertyOnce . expectFailure . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- We modify the hash slightly so that the hash check fails
    alteredHash <- H.evalMaybe $ tamperBase16Hash exampleGovActionAnchorHash2
    -- We run the test with the modified hash
    base_golden_governance_action_create_constitution
      exampleGovActionAnchorHash1
      alteredHash
      tempDir

hprop_golden_governance_action_create_constitution :: Property
hprop_golden_governance_action_create_constitution =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir ->
    base_golden_governance_action_create_constitution
      exampleGovActionAnchorHash1
      exampleGovActionAnchorHash2
      tempDir

base_golden_governance_action_create_constitution
  :: (MonadBaseControl IO m, MonadTest m, MonadIO m, MonadCatch m)
  => String
  -> String
  -> FilePath
  -> m ()
base_golden_governance_action_create_constitution hash1 hash2 tempDir = do
  stakeAddressVKeyFile <- noteTempFile tempDir "stake-address.vkey"
  stakeAddressSKeyFile <- noteTempFile tempDir "stake-address.skey"

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

  actionFile <- noteTempFile tempDir "create-constitution.action"
  redactedActionFile <- noteTempFile tempDir "create-constitution.action.redacted"

  let relativeUrl1 = ["ipfs", exampleGovActionAnchorIpfsHash1]
  let relativeUrl2 = ["ipfs", exampleGovActionAnchorIpfsHash2]

  -- Create temporary HTTP server with files required by the call to `cardano-cli`
  -- In this case, the server emulates an IPFS gateway
  serveFilesWhile
    [ (relativeUrl1, exampleGovActionAnchorPathGolden1)
    , (relativeUrl2, exampleGovActionAnchorPathGolden2)
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
            , "ipfs://" ++ exampleGovActionAnchorIpfsHash1
            , "--check-anchor-data"
            , "--governance-action-deposit"
            , "10"
            , "--deposit-return-stake-verification-key-file"
            , stakeAddressVKeyFile
            , "--out-file"
            , actionFile
            , "--constitution-url"
            , "ipfs://" ++ exampleGovActionAnchorIpfsHash2
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
  propertyOnce . expectFailure . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- We modify the hash slightly so that the hash check fails
    alteredHash <- H.evalMaybe $ tamperBase16Hash exampleGovActionAnchorHash1
    -- We run the test with the modified hash
    base_golden_conway_governance_action_view_update_committee_yaml
      alteredHash
      tempDir

hprop_golden_conway_governance_action_view_update_committee_yaml :: Property
hprop_golden_conway_governance_action_view_update_committee_yaml =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir ->
    base_golden_conway_governance_action_view_update_committee_yaml exampleGovActionAnchorHash1 tempDir

base_golden_conway_governance_action_view_update_committee_yaml
  :: (MonadBaseControl IO m, MonadTest m, MonadIO m, MonadCatch m) => String -> FilePath -> m ()
base_golden_conway_governance_action_view_update_committee_yaml hash tempDir = do
  stakeAddressVKeyFile <- H.note "test/cardano-cli-golden/files/input/governance/stake-address.vkey"

  actionFile <- noteTempFile tempDir "action"

  let relativeUrl = ["ipfs", exampleGovActionAnchorIpfsHash1]

  -- Create temporary HTTP server with files required by the call to `cardano-cli`
  -- In this case, the server emulates an IPFS gateway
  serveFilesWhile
    [(relativeUrl, exampleGovActionAnchorPathGolden1)]
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
            , "ipfs://" ++ exampleGovActionAnchorIpfsHash1
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
  propertyOnce . expectFailure . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- We modify the hash slightly so that the hash check fails
    alteredHash <- H.evalMaybe $ tamperBase16Hash exampleGovActionAnchorHash1
    -- We run the test with the modified hash
    base_golden_conway_governance_action_view_create_info_json_outfile
      alteredHash
      tempDir

hprop_golden_conway_governance_action_view_create_info_json_outfile :: Property
hprop_golden_conway_governance_action_view_create_info_json_outfile =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir ->
    base_golden_conway_governance_action_view_create_info_json_outfile
      exampleGovActionAnchorHash1
      tempDir

base_golden_conway_governance_action_view_create_info_json_outfile
  :: (MonadBaseControl IO m, MonadTest m, MonadIO m, MonadCatch m) => String -> FilePath -> m ()
base_golden_conway_governance_action_view_create_info_json_outfile hash tempDir = do
  stakeAddressVKeyFile <- H.note "test/cardano-cli-golden/files/input/governance/stake-address.vkey"

  actionFile <- noteTempFile tempDir "action"

  let relativeUrl = ["ipfs", exampleGovActionAnchorIpfsHash1]

  -- Create temporary HTTP server with files required by the call to `cardano-cli`
  -- In this case, the server emulates an IPFS gateway
  serveFilesWhile
    [(relativeUrl, exampleGovActionAnchorPathGolden1)]
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
            , "ipfs://" ++ exampleGovActionAnchorIpfsHash1
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
  propertyOnce . expectFailure . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- We modify the hash slightly so that the hash check fails
    alteredHash <- H.evalMaybe $ tamperBase16Hash exampleGovActionAnchorHash1
    -- We run the test with the modified hash
    base_golden_governanceActionCreateNoConfidence
      alteredHash
      tempDir

hprop_golden_governanceActionCreateNoConfidence :: Property
hprop_golden_governanceActionCreateNoConfidence =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir ->
    base_golden_governanceActionCreateNoConfidence exampleGovActionAnchorHash1 tempDir

base_golden_governanceActionCreateNoConfidence
  :: (MonadBaseControl IO m, MonadTest m, MonadIO m, MonadCatch m) => String -> FilePath -> m ()
base_golden_governanceActionCreateNoConfidence hash tempDir = do
  stakeAddressVKeyFile <-
    noteInputFile "test/cardano-cli-golden/files/input/governance/stake-address.vkey"

  actionFile <- noteTempFile tempDir "action"

  let relativeUrl = ["ipfs", exampleGovActionAnchorIpfsHash1]

  -- Create temporary HTTP server with files required by the call to `cardano-cli`
  -- In this case, the server emulates an IPFS gateway
  serveFilesWhile
    [(relativeUrl, exampleGovActionAnchorPathGolden1)]
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
            , "ipfs://" ++ exampleGovActionAnchorIpfsHash1
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
  propertyOnce . expectFailure . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- We modify the hash slightly so that the hash check fails
    alteredHash <- H.evalMaybe $ tamperBase16Hash exampleGovActionAnchorHash1
    -- We run the test with the modified hash
    base_golden_conway_governance_action_create_protocol_parameters_update
      alteredHash
      tempDir

hprop_golden_conway_governance_action_create_protocol_parameters_update :: Property
hprop_golden_conway_governance_action_create_protocol_parameters_update =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir ->
    base_golden_conway_governance_action_create_protocol_parameters_update
      exampleGovActionAnchorHash1
      tempDir

base_golden_conway_governance_action_create_protocol_parameters_update
  :: (MonadBaseControl IO m, MonadTest m, MonadIO m, MonadCatch m) => String -> FilePath -> m ()
base_golden_conway_governance_action_create_protocol_parameters_update hash tempDir = do
  stakeAddressVKeyFile <- H.note "test/cardano-cli-golden/files/input/governance/stake-address.vkey"
  -- different versions of https://github.com/IntersectMBO/plutus/blob/master/plutus-core/cost-model/data/builtinCostModel.json
  -- transformed and compiled together
  costModelsFile <- H.note "test/cardano-cli-golden/files/input/governance/costmodels.json"

  actionFile <- noteTempFile tempDir "action"

  let relativeUrl = ["ipfs", exampleGovActionAnchorIpfsHash1]

  -- Create temporary HTTP server with files required by the call to `cardano-cli`
  -- In this case, the server emulates an IPFS gateway
  serveFilesWhile
    [(relativeUrl, exampleGovActionAnchorPathGolden1)]
    ( \port -> do
        void $
          execCardanoCLIWithEnvVars
            [("IPFS_GATEWAY_URI", "http://localhost:" ++ show port ++ "/")]
            [ "conway"
            , "governance"
            , "action"
            , "create-protocol-parameters-update"
            , "--anchor-url"
            , "ipfs://" ++ exampleGovActionAnchorIpfsHash1
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
  propertyOnce . expectFailure . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- We modify the hash slightly so that the hash check fails
    alteredHash <- H.evalMaybe $ tamperBase16Hash exampleGovActionAnchorHash1
    -- We run the test with the modified hash
    base_golden_conway_governance_action_create_hardfork
      alteredHash
      tempDir

hprop_golden_conway_governance_action_create_hardfork :: Property
hprop_golden_conway_governance_action_create_hardfork =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir ->
    base_golden_conway_governance_action_create_hardfork exampleGovActionAnchorHash1 tempDir

base_golden_conway_governance_action_create_hardfork
  :: (MonadBaseControl IO m, MonadTest m, MonadIO m, MonadCatch m) => String -> FilePath -> m ()
base_golden_conway_governance_action_create_hardfork hash tempDir = do
  stakeAddressVKeyFile <- H.note "test/cardano-cli-golden/files/input/governance/stake-address.vkey"

  actionFile <- noteTempFile tempDir "action"

  let relativeUrl = ["ipfs", exampleGovActionAnchorIpfsHash1]

  -- Create temporary HTTP server with files required by the call to `cardano-cli`
  -- In this case, the server emulates an IPFS gateway
  serveFilesWhile
    [(relativeUrl, exampleGovActionAnchorPathGolden1)]
    ( \port -> do
        void $
          execCardanoCLIWithEnvVars
            [("IPFS_GATEWAY_URI", "http://localhost:" ++ show port ++ "/")]
            [ "conway"
            , "governance"
            , "action"
            , "create-hardfork"
            , "--anchor-url"
            , "ipfs://" ++ exampleGovActionAnchorIpfsHash1
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
