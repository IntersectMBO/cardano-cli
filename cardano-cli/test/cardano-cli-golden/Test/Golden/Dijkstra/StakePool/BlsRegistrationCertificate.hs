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
