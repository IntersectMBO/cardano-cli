{-# LANGUAGE FlexibleContexts #-}

module Test.Cli.Shelley.Certificates.StakePool where

import           Cardano.Api (MonadIO)

import           Control.Monad (void)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Trans.Control (MonadBaseControl)

import           Test.Cardano.CLI.Hash (serveFilesWhile, tamperBase16Hash)
import           Test.Cardano.CLI.Util (execCardanoCLI, execCardanoCLIWithEnvVars, expectFailure,
                   noteTempFile, propertyOnce)

import           Hedgehog (MonadTest)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import           Hedgehog.Internal.Property (Property)

exampleStakePoolMetadataHash :: String
exampleStakePoolMetadataHash = "8241de08075886a7d09c847c9bbd1719459dac0bd0a2f085e673611ebb9a5965"

exampleStakePoolMetadataPathTest :: String
exampleStakePoolMetadataPathTest = "test/cardano-cli-test/files/input/example_stake_pool_metadata.json"

exampleStakePoolMetadataIpfsHash :: String
exampleStakePoolMetadataIpfsHash = "QmR1HAT4Hb4HjjqcgoXwupYXMF6t8h7MoSP24HMfV8t38a"

-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/stake pool certificate hash check wrong hash fails/"'@
hprop_stake_pool_certificate_hash_check_wrong_hash_fails :: Property
hprop_stake_pool_certificate_hash_check_wrong_hash_fails =
  propertyOnce . expectFailure . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- We modify the hash slightly so that the hash check fails
    alteredHash <- H.evalMaybe $ tamperBase16Hash exampleStakePoolMetadataHash
    -- We run the test with the modified hash
    baseStakePoolCertificateHashCheck
      alteredHash
      tempDir

-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/stake pool certificate hash check right hash works/"'@
hprop_stake_pool_certificate_hash_check_right_hash_works :: Property
hprop_stake_pool_certificate_hash_check_right_hash_works =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir ->
    baseStakePoolCertificateHashCheck exampleStakePoolMetadataHash tempDir

baseStakePoolCertificateHashCheck
  :: (MonadBaseControl IO m, MonadTest m, MonadIO m, MonadCatch m)
  => String
  -- ^ The hash to check against. Changing this value allows us to test the
  -- behavior of the command both when the hash is correct and when it is incorrect
  -- reusing the same code.
  -> FilePath
  -- ^ Temporary directory for files generated during the test
  -> m ()
baseStakePoolCertificateHashCheck hash tempDir = do
  -- Key filepaths
  coldVerKey <- noteTempFile tempDir "cold-verification-key-file"
  coldSignKey <- noteTempFile tempDir "cold-signing-key-file"
  operationalCertCounter <- noteTempFile tempDir "operational-certificate-counter-file"
  vrfVerKey <- noteTempFile tempDir "vrf-verification-key-file"
  vrfSignKey <- noteTempFile tempDir "vrf-signing-key-file"
  poolRewardAccountAndOwnerVerKey <- noteTempFile tempDir "reward-account-verification-key-file"
  poolRewardAccountSignKey <- noteTempFile tempDir "reward-account-signing-key-file"
  registrationCertificate <- noteTempFile tempDir "stake-pool-registration-certificate"

  -- Create cold key pair
  void $
    execCardanoCLI
      [ "latest"
      , "node"
      , "key-gen"
      , "--cold-verification-key-file"
      , coldVerKey
      , "--cold-signing-key-file"
      , coldSignKey
      , "--operational-certificate-issue-counter"
      , operationalCertCounter
      ]

  H.assertFilesExist [coldSignKey, coldVerKey, operationalCertCounter]

  -- Generate stake key pair
  void $
    execCardanoCLI
      [ "latest"
      , "stake-address"
      , "key-gen"
      , "--verification-key-file"
      , poolRewardAccountAndOwnerVerKey
      , "--signing-key-file"
      , poolRewardAccountSignKey
      ]

  H.assertFilesExist [poolRewardAccountAndOwnerVerKey, poolRewardAccountSignKey]

  -- Generate vrf verification key
  void $
    execCardanoCLI
      [ "latest"
      , "node"
      , "key-gen-VRF"
      , "--verification-key-file"
      , vrfVerKey
      , "--signing-key-file"
      , vrfSignKey
      ]

  H.assertFilesExist [vrfSignKey, vrfVerKey]

  let relativeUrl = ["ipfs", exampleStakePoolMetadataIpfsHash]

  -- Create temporary HTTP server with files required by the call to `cardano-cli`
  -- In this case, the server emulates an IPFS gateway
  serveFilesWhile
    [ (relativeUrl, exampleStakePoolMetadataPathTest)
    ]
    ( \port -> do
        -- Create stake pool registration certificate
        void $
          execCardanoCLIWithEnvVars
            [("IPFS_GATEWAY_URI", "http://localhost:" ++ show port ++ "/")]
            [ "babbage"
            , "stake-pool"
            , "registration-certificate"
            , "--cold-verification-key-file"
            , coldVerKey
            , "--vrf-verification-key-file"
            , vrfVerKey
            , "--mainnet"
            , "--pool-cost"
            , "1000"
            , "--pool-pledge"
            , "5000"
            , "--pool-margin"
            , "0.1"
            , "--pool-reward-account-verification-key-file"
            , poolRewardAccountAndOwnerVerKey
            , "--pool-owner-stake-verification-key-file"
            , poolRewardAccountAndOwnerVerKey
            , "--metadata-url"
            , "ipfs://" ++ exampleStakePoolMetadataIpfsHash
            , "--metadata-hash"
            , hash
            , "--check-metadata-hash"
            , "--out-file"
            , registrationCertificate
            ]
    )

-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/stake pool metadata hash url wrong hash fails/"'@
hprop_stake_pool_metadata_hash_url_wrong_hash_fails :: Property
hprop_stake_pool_metadata_hash_url_wrong_hash_fails =
  propertyOnce . expectFailure $ do
    -- We modify the hash slightly so that the hash check fails
    alteredHash <- H.evalMaybe $ tamperBase16Hash exampleStakePoolMetadataHash
    -- We run the test with the modified hash
    baseStakePoolMetadataHashUrl alteredHash

-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/stake pool metadata hash url correct hash/"'@
hprop_stake_pool_metadata_hash_url_correct_hash :: Property
hprop_stake_pool_metadata_hash_url_correct_hash =
  propertyOnce $ baseStakePoolMetadataHashUrl exampleStakePoolMetadataHash

baseStakePoolMetadataHashUrl
  :: (MonadBaseControl IO m, MonadTest m, MonadIO m, MonadCatch m)
  => String
  -- ^ The hash to check against. Changing this value allows us to test the
  -- behavior of the command both when the hash is correct and when it is incorrect
  -- reusing the same code.
  -> m ()
baseStakePoolMetadataHashUrl hash = do
  let relativeUrl = ["ipfs", exampleStakePoolMetadataIpfsHash]

  -- Create temporary HTTP server with files required by the call to `cardano-cli`
  -- In this case, the server emulates an IPFS gateway
  serveFilesWhile
    [ (relativeUrl, exampleStakePoolMetadataPathTest)
    ]
    ( \port -> do
        void $
          execCardanoCLIWithEnvVars
            [("IPFS_GATEWAY_URI", "http://localhost:" ++ show port ++ "/")]
            [ "conway"
            , "stake-pool"
            , "metadata-hash"
            , "--pool-metadata-url"
            , "ipfs://" ++ exampleStakePoolMetadataIpfsHash
            , "--expected-hash"
            , hash
            ]
    )
