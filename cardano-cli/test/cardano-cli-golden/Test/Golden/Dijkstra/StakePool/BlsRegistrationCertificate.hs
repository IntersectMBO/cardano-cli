{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Golden.Dijkstra.StakePool.BlsRegistrationCertificate where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Data.Coerce (coerce)
import Data.List (find, isInfixOf, isPrefixOf)
import System.Exit (ExitCode (..))

import Test.Cardano.CLI.Util
  ( execDetailCardanoCLI
  , noteInputFile
  , noteTempFile
  , propertyOnce
  , watchdogProp
  )

import Hedgehog (MonadTest, Property)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H

-- | The era the commands under test run in.
type TestEra = Exp.DijkstraEra

registrationCertificateInput :: FilePath
registrationCertificateInput =
  "test/cardano-cli-golden/files/input/dijkstra/stake_pool/registration_certificate.json"

alwaysAbstainDelegationCertificateInput :: FilePath
alwaysAbstainDelegationCertificateInput =
  "test/cardano-cli-golden/files/input/conway/stake_address/always_abstain_deleg.cert"

blsSigningKeyInput :: FilePath
blsSigningKeyInput = "test/cardano-cli-golden/files/input/shelley/keys/bls_keys/signing_key"

secondBlsSigningKeyInput :: FilePath
secondBlsSigningKeyInput = "test/cardano-cli-golden/files/input/shelley/keys/bls_keys/signing_key_2"

richRegistrationCertificateInput :: FilePath
richRegistrationCertificateInput =
  "test/cardano-cli-golden/files/input/dijkstra/stake_pool/registration_certificate_rich_params.json"

conwayRegistrationCertificateInput :: FilePath
conwayRegistrationCertificateInput =
  "test/cardano-cli-golden/files/input/conway/stake_pool/registration_certificate_rich_params.json"

-- | Rotating the BLS key of a certificate with a margin, two relays of different kinds and pool
-- metadata keeps every other parameter.
hprop_golden_dijkstra_stake_pool_bls_registration_certificate :: Property
hprop_golden_dijkstra_stake_pool_bls_registration_certificate =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    registrationCertFile <- noteInputFile richRegistrationCertificateInput
    blsSigningKeyFile <- noteInputFile secondBlsSigningKeyInput
    outFile <- noteTempFile tempDir "bls-registration.cert"

    (exitCode, _stdout, stderr) <-
      execDetailCardanoCLI $ blsRegistrationCertificateArgs registrationCertFile blsSigningKeyFile outFile

    exitCode H.=== ExitSuccess
    oldKey <- H.nothingFail $ findLineValue "Old BLS key: " stderr
    newKey <- H.nothingFail $ findLineValue "New BLS key: " stderr
    H.assertWith (oldKey, newKey) $ uncurry (/=)

    goldenFile <-
      H.note "test/cardano-cli-golden/files/golden/dijkstra/stake_pool/bls_registration_certificate.json"

    H.diffFileVsGoldenFile outFile goldenFile

-- | Right after the hard fork a pool only has a Conway registration certificate, which has no BLS key.
-- The result is a Dijkstra certificate that keeps every other parameter.
hprop_golden_dijkstra_stake_pool_bls_registration_certificate_from_conway :: Property
hprop_golden_dijkstra_stake_pool_bls_registration_certificate_from_conway =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    conwayCertFile <- noteInputFile conwayRegistrationCertificateInput
    blsSigningKeyFile <- noteInputFile secondBlsSigningKeyInput
    outFile <- noteTempFile tempDir "bls-registration.cert"

    (exitCode, _stdout, stderr) <-
      execDetailCardanoCLI $ blsRegistrationCertificateArgs conwayCertFile blsSigningKeyFile outFile

    exitCode H.=== ExitSuccess
    H.assertWith stderr ("Old BLS key: none (first registration)" `isInfixOf`)

    conwayParams <- readPoolParams @Exp.ConwayEra conwayCertFile
    newParams <- readPoolParams @TestEra outFile
    newParams{L.sppBlsKey = L.SNothing} H.=== coerce conwayParams
    H.assertWith (L.sppBlsKey newParams) (/= L.SNothing)

    -- Same parameters and BLS key as the Dijkstra input, so the same golden file applies.
    goldenFile <-
      H.note "test/cardano-cli-golden/files/golden/dijkstra/stake_pool/bls_registration_certificate.json"

    H.diffFileVsGoldenFile outFile goldenFile

-- | Certificates written by earlier releases carry the text envelope type "CertificateShelley".
hprop_dijkstra_stake_pool_bls_registration_certificate_legacy_envelope_type :: Property
hprop_dijkstra_stake_pool_bls_registration_certificate_legacy_envelope_type =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    legacyCertFile <-
      noteInputFile
        "test/cardano-cli-golden/files/input/shelley/certificates/stake_pool_registration_certificate"
    blsSigningKeyFile <- noteInputFile blsSigningKeyInput
    outFile <- noteTempFile tempDir "bls-registration.cert"

    (exitCode, _stdout, stderr) <-
      execDetailCardanoCLI $ blsRegistrationCertificateArgs legacyCertFile blsSigningKeyFile outFile

    exitCode H.=== ExitSuccess
    H.assertWith stderr ("Old BLS key: none (first registration)" `isInfixOf`)

-- | A certificate without a BLS key (a pool's first registration of one) gets the new key, and
-- every other parameter stays.
hprop_dijkstra_stake_pool_bls_registration_certificate_first_registration :: Property
hprop_dijkstra_stake_pool_bls_registration_certificate_first_registration =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    withBlsCertFile <- noteInputFile registrationCertificateInput
    blsSigningKeyFile <- noteInputFile blsSigningKeyInput
    noBlsCertFile <- noteTempFile tempDir "no-bls.cert"
    outFile <- noteTempFile tempDir "bls-registration.cert"

    oldParams <- readPoolParams @TestEra withBlsCertFile
    writeCertificate @TestEra noBlsCertFile $ oldParams{L.sppBlsKey = L.SNothing}

    (exitCode, _stdout, stderr) <-
      execDetailCardanoCLI $ blsRegistrationCertificateArgs noBlsCertFile blsSigningKeyFile outFile

    exitCode H.=== ExitSuccess
    H.assertWith stderr ("Old BLS key: none (first registration)" `isInfixOf`)
    H.assertWith stderr ("Pool ID: pool1" `isInfixOf`)

    newParams <- readPoolParams @TestEra outFile
    -- The input key is the one in the input certificate, so the result equals the original.
    newParams H.=== oldParams

-- | Using the key that is already registered still writes the certificate, with a warning.
hprop_dijkstra_stake_pool_bls_registration_certificate_same_key :: Property
hprop_dijkstra_stake_pool_bls_registration_certificate_same_key =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    registrationCertFile <- noteInputFile registrationCertificateInput
    blsSigningKeyFile <- noteInputFile blsSigningKeyInput
    outFile <- noteTempFile tempDir "bls-registration.cert"

    (exitCode, _stdout, stderr) <-
      execDetailCardanoCLI $ blsRegistrationCertificateArgs registrationCertFile blsSigningKeyFile outFile

    exitCode H.=== ExitSuccess
    H.assertWith stderr ("WARNING" `isInfixOf`)
    H.diffFileVsGoldenFile outFile registrationCertificateInput

-- | The input must be a stake pool registration certificate.
hprop_dijkstra_stake_pool_bls_registration_certificate_not_a_pool_certificate :: Property
hprop_dijkstra_stake_pool_bls_registration_certificate_not_a_pool_certificate =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    otherCertFile <-
      noteInputFile alwaysAbstainDelegationCertificateInput
    blsSigningKeyFile <- noteInputFile blsSigningKeyInput
    outFile <- noteTempFile tempDir "bls-registration.cert"

    (exitCode, _stdout, stderr) <-
      execDetailCardanoCLI $ blsRegistrationCertificateArgs otherCertFile blsSigningKeyFile outFile

    exitCode H.=== ExitFailure 1
    H.assertWith stderr ("is not a stake pool registration certificate" `isInfixOf`)

poolStateInput :: FilePath
poolStateInput = "test/cardano-cli-golden/files/input/dijkstra/stake_pool/pool_state.json"

-- | The current parameters of a pool with a pending update: the certificate equals the one made from
-- the certificate that the pool state was built from. Only the pending update is warned about.
hprop_golden_dijkstra_stake_pool_bls_registration_certificate_pool_state_current :: Property
hprop_golden_dijkstra_stake_pool_bls_registration_certificate_pool_state_current =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    poolStateFile <- noteInputFile poolStateInput
    blsSigningKeyFile <- noteInputFile secondBlsSigningKeyInput
    outFile <- noteTempFile tempDir "bls-registration.cert"

    (exitCode, _stdout, stderr) <-
      execDetailCardanoCLI $
        blsRegistrationCertificateFromPoolStateArgs
          poolStateFile
          pendingUpdatePoolId
          blsSigningKeyFile
          outFile
          ["--current-params", "--testnet-magic", "42"]

    exitCode H.=== ExitSuccess
    H.assertWith stderr ("Pool ID: pool1" `isInfixOf`)
    oldKey <- H.nothingFail $ findLineValue "Old BLS key: " stderr
    newKey <- H.nothingFail $ findLineValue "New BLS key: " stderr
    H.assertWith (oldKey, newKey) $ uncurry (/=)
    H.assertWith stderr ("WARNING: The pool has a pending parameter update." `isInfixOf`)
    H.assertWith stderr (not . ("scheduled to retire" `isInfixOf`))

    goldenFile <-
      H.note "test/cardano-cli-golden/files/golden/dijkstra/stake_pool/bls_registration_certificate.json"

    H.diffFileVsGoldenFile outFile goldenFile

-- | The pending parameters carry their own network and replace the current ones, so there is
-- nothing to warn about.
hprop_golden_dijkstra_stake_pool_bls_registration_certificate_pool_state_pending :: Property
hprop_golden_dijkstra_stake_pool_bls_registration_certificate_pool_state_pending =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    poolStateFile <- noteInputFile poolStateInput
    blsSigningKeyFile <- noteInputFile secondBlsSigningKeyInput
    outFile <- noteTempFile tempDir "bls-registration.cert"

    (exitCode, _stdout, stderr) <-
      execDetailCardanoCLI $
        blsRegistrationCertificateFromPoolStateArgs
          poolStateFile
          pendingUpdatePoolId
          blsSigningKeyFile
          outFile
          ["--pending-params"]

    exitCode H.=== ExitSuccess
    H.assertWith stderr ("Pool ID: pool1" `isInfixOf`)
    H.assertWith stderr (not . ("WARNING" `isInfixOf`))

    richParams <- readPoolParams @TestEra richRegistrationCertificateInput
    newParams <- readPoolParams @TestEra outFile
    H.assertWith (L.sppPledge newParams, L.sppPledge richParams) $ uncurry (/=)

    goldenFile <-
      H.note
        "test/cardano-cli-golden/files/golden/dijkstra/stake_pool/bls_registration_certificate_pending_params.json"

    H.diffFileVsGoldenFile outFile goldenFile

-- | A pool that retires and has no pending update is warned about the retirement only.
hprop_dijkstra_stake_pool_bls_registration_certificate_pool_state_retiring_pool :: Property
hprop_dijkstra_stake_pool_bls_registration_certificate_pool_state_retiring_pool =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    poolStateFile <- noteInputFile poolStateInput
    blsSigningKeyFile <- noteInputFile secondBlsSigningKeyInput
    outFile <- noteTempFile tempDir "bls-registration.cert"

    (exitCode, _stdout, stderr) <-
      execDetailCardanoCLI $
        blsRegistrationCertificateFromPoolStateArgs
          poolStateFile
          retiringPoolId
          blsSigningKeyFile
          outFile
          ["--current-params", "--testnet-magic", "42"]

    exitCode H.=== ExitSuccess
    H.assertWith stderr ("WARNING: The pool is scheduled to retire in epoch 300." `isInfixOf`)
    H.assertWith stderr (not . ("pending parameter update" `isInfixOf`))

-- | A pool that neither retires nor has a pending update gives no warnings, and has no BLS key yet.
hprop_dijkstra_stake_pool_bls_registration_certificate_pool_state_plain_pool :: Property
hprop_dijkstra_stake_pool_bls_registration_certificate_pool_state_plain_pool =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    poolStateFile <- noteInputFile poolStateInput
    blsSigningKeyFile <- noteInputFile secondBlsSigningKeyInput
    outFile <- noteTempFile tempDir "bls-registration.cert"

    (exitCode, _stdout, stderr) <-
      execDetailCardanoCLI $
        blsRegistrationCertificateFromPoolStateArgs
          poolStateFile
          plainPoolId
          blsSigningKeyFile
          outFile
          ["--current-params", "--testnet-magic", "42"]

    exitCode H.=== ExitSuccess
    H.assertWith stderr ("Old BLS key: none (first registration)" `isInfixOf`)
    H.assertWith stderr (not . ("WARNING" `isInfixOf`))

    richParams <- readPoolParams @TestEra richRegistrationCertificateInput
    newParams <- readPoolParams @TestEra outFile
    L.sppVrf newParams H.=== L.sppVrf richParams
    L.sppAccountAddress newParams H.=== L.sppAccountAddress richParams

-- | The pool must be in the pool state file.
hprop_dijkstra_stake_pool_bls_registration_certificate_pool_state_pool_missing :: Property
hprop_dijkstra_stake_pool_bls_registration_certificate_pool_state_pool_missing =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    poolStateFile <- noteInputFile poolStateInput
    blsSigningKeyFile <- noteInputFile secondBlsSigningKeyInput
    outFile <- noteTempFile tempDir "bls-registration.cert"
    let missingPoolId = replicate 56 '0'
        missingPoolBech32 = "pool1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq8a7a2d"

    (exitCode, _stdout, stderr) <-
      execDetailCardanoCLI $
        blsRegistrationCertificateFromPoolStateArgs
          poolStateFile
          missingPoolId
          blsSigningKeyFile
          outFile
          ["--pending-params"]

    exitCode H.=== ExitFailure 1
    H.assertWith stderr (("has no pool " <> missingPoolBech32) `isInfixOf`)
    H.assertWith stderr (poolStateFile `isInfixOf`)

-- | A pool without a pending update cannot give pending parameters.
hprop_dijkstra_stake_pool_bls_registration_certificate_pool_state_no_pending_update :: Property
hprop_dijkstra_stake_pool_bls_registration_certificate_pool_state_no_pending_update =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    poolStateFile <- noteInputFile poolStateInput
    blsSigningKeyFile <- noteInputFile secondBlsSigningKeyInput
    outFile <- noteTempFile tempDir "bls-registration.cert"

    (exitCode, _stdout, stderr) <-
      execDetailCardanoCLI $
        blsRegistrationCertificateFromPoolStateArgs
          poolStateFile
          plainPoolId
          blsSigningKeyFile
          outFile
          ["--pending-params"]

    exitCode H.=== ExitFailure 1
    H.assertWith stderr ("has no pending parameter update" `isInfixOf`)
    H.assertWith stderr (poolStateFile `isInfixOf`)

-- | The pool state file must be the JSON of @query pool-state@.
hprop_dijkstra_stake_pool_bls_registration_certificate_pool_state_malformed_file :: Property
hprop_dijkstra_stake_pool_bls_registration_certificate_pool_state_malformed_file =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    notPoolStateFile <- noteInputFile registrationCertificateInput
    blsSigningKeyFile <- noteInputFile secondBlsSigningKeyInput
    outFile <- noteTempFile tempDir "bls-registration.cert"

    (exitCode, _stdout, stderr) <-
      execDetailCardanoCLI $
        blsRegistrationCertificateFromPoolStateArgs
          notPoolStateFile
          pendingUpdatePoolId
          blsSigningKeyFile
          outFile
          ["--pending-params"]

    exitCode H.=== ExitFailure 1
    H.assertWith stderr (notPoolStateFile `isInfixOf`)

-- | A certificate file and a pool state file are alternatives.
hprop_dijkstra_stake_pool_bls_registration_certificate_both_parameter_sources :: Property
hprop_dijkstra_stake_pool_bls_registration_certificate_both_parameter_sources =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    registrationCertFile <- noteInputFile richRegistrationCertificateInput
    poolStateFile <- noteInputFile poolStateInput
    blsSigningKeyFile <- noteInputFile secondBlsSigningKeyInput
    outFile <- noteTempFile tempDir "bls-registration.cert"

    (exitCode, _stdout, stderr) <-
      execDetailCardanoCLI $
        blsRegistrationCertificateArgs registrationCertFile blsSigningKeyFile outFile
          <> ["--pool-state-file", poolStateFile, "--stake-pool-id", pendingUpdatePoolId, "--pending-params"]

    exitCode H.=== ExitFailure 1
    H.assertWith stderr ("Invalid option" `isInfixOf`)

-- | The network belongs to the current parameters, so the pending ones do not accept it.
hprop_dijkstra_stake_pool_bls_registration_certificate_pending_params_with_network :: Property
hprop_dijkstra_stake_pool_bls_registration_certificate_pending_params_with_network =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    poolStateFile <- noteInputFile poolStateInput
    blsSigningKeyFile <- noteInputFile secondBlsSigningKeyInput
    outFile <- noteTempFile tempDir "bls-registration.cert"

    (exitCode, _stdout, stderr) <-
      execDetailCardanoCLI $
        blsRegistrationCertificateFromPoolStateArgs
          poolStateFile
          pendingUpdatePoolId
          blsSigningKeyFile
          outFile
          ["--pending-params", "--testnet-magic", "42"]

    exitCode H.=== ExitFailure 1
    H.assertWith stderr ("Invalid option" `isInfixOf`)

-- | The hex pool IDs in the pool state file: a pool with a pending update, a retiring pool, and a
-- plain pool, which uses the hash of an owner of the first.
pendingUpdatePoolId, retiringPoolId, plainPoolId :: String
pendingUpdatePoolId = "4124a0c2ba8a7ba05824b3df23f706a1c8b3239446ab16709ae85aa1"
retiringPoolId = "5c1b0a7e9d3f4268a1b2c3d4e5f607182930a4b5c6d7e8f90123abcd"
plainPoolId = "749440f878a5ba2ac3ab03dbdec6ae3ad5c89f06fc9c031611947803"

-- | The text after the prefix on the first stderr line that starts with it.
findLineValue :: String -> String -> Maybe String
findLineValue prefix = fmap (drop $ length prefix) . find (prefix `isPrefixOf`) . lines

blsRegistrationCertificateArgs :: FilePath -> FilePath -> FilePath -> [String]
blsRegistrationCertificateArgs certFile blsSigningKeyFile outFile =
  [ "dijkstra"
  , "stake-pool"
  , "bls-registration-certificate"
  , "offline"
  , "--registration-certificate-file"
  , certFile
  , "--bls-signing-key-file"
  , blsSigningKeyFile
  , "--out-file"
  , outFile
  ]

blsRegistrationCertificateFromPoolStateArgs
  :: FilePath -> String -> FilePath -> FilePath -> [String] -> [String]
blsRegistrationCertificateFromPoolStateArgs poolStateFile poolId blsSigningKeyFile outFile choice =
  [ "dijkstra"
  , "stake-pool"
  , "bls-registration-certificate"
  , "offline"
  , "--pool-state-file"
  , poolStateFile
  , "--stake-pool-id"
  , poolId
  , "--bls-signing-key-file"
  , blsSigningKeyFile
  , "--out-file"
  , outFile
  ]
    <> choice

readPoolParams
  :: forall era m
   . (Exp.IsEra era, MonadTest m, MonadIO m)
  => FilePath -> m (L.StakePoolParams (Exp.LedgerEra era))
readPoolParams file = Exp.obtainCommonConstraints (Exp.useEra @era) $ do
  Exp.Certificate cert <-
    H.leftFailM . H.evalIO $
      readFileTextEnvelope @(Exp.Certificate (Exp.LedgerEra era)) (File file)
  H.evalMaybe $ L.getRegPoolTxCert cert

writeCertificate
  :: forall era m
   . (Exp.IsEra era, MonadTest m, MonadIO m)
  => FilePath -> L.StakePoolParams (Exp.LedgerEra era) -> m ()
writeCertificate file params = Exp.obtainCommonConstraints (Exp.useEra @era) $ do
  let cert :: Exp.Certificate (Exp.LedgerEra era)
      cert = Exp.makeStakePoolRegistrationCertificate params
      write :: IO (Either (FileError ()) ())
      write =
        writeLazyByteStringFile
          (File file)
          (textEnvelopeToJSON (Just "Stake Pool Registration Certificate") cert)
  H.leftFailM $ H.evalIO write
