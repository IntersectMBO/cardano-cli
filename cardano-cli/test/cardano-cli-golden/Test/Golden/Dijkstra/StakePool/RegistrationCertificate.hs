module Test.Golden.Dijkstra.StakePool.RegistrationCertificate where

import Control.Monad (void)
import Data.List (isInfixOf)
import System.Exit (ExitCode (..))

import Test.Cardano.CLI.Util
  ( execCardanoCLI
  , execDetailCardanoCLI
  , noteInputFile
  , noteTempFile
  , propertyOnce
  , watchdogProp
  )

import Hedgehog (Property)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H

-- | From Dijkstra onwards a pool registers a BLS voting key, so the emitted
-- certificate carries the verification key together with its proof of
-- possession. The BLS signing key is a fixed input and the proof of possession
-- is deterministic, so the whole certificate can be compared against a golden
-- file.
hprop_golden_dijkstra_stake_pool_registration_certificate :: Property
hprop_golden_dijkstra_stake_pool_registration_certificate =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    operatorVerificationKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/node-pool/operator.vkey"
    vrfVerificationKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/node-pool/vrf.vkey"
    ownerVerificationKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/node-pool/owner.vkey"
    blsSigningKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/bls_keys/signing_key"
    registrationCertFile <- noteTempFile tempDir "registration.cert"

    void $
      execCardanoCLI
        [ "dijkstra"
        , "stake-pool"
        , "registration-certificate"
        , "--testnet-magic"
        , "42"
        , "--pool-pledge"
        , "0"
        , "--pool-cost"
        , "0"
        , "--pool-margin"
        , "0"
        , "--cold-verification-key-file"
        , operatorVerificationKeyFile
        , "--vrf-verification-key-file"
        , vrfVerificationKeyFile
        , "--bls-signing-key-file"
        , blsSigningKeyFile
        , "--pool-reward-account-verification-key-file"
        , ownerVerificationKeyFile
        , "--pool-owner-stake-verification-key-file"
        , ownerVerificationKeyFile
        , "--out-file"
        , registrationCertFile
        ]

    goldenFile <-
      H.note "test/cardano-cli-golden/files/golden/dijkstra/stake_pool/registration_certificate.json"

    H.diffFileVsGoldenFile registrationCertFile goldenFile

-- | The BLS voting key is mandatory in Dijkstra.
hprop_golden_dijkstra_stake_pool_registration_certificate_missing_bls_key :: Property
hprop_golden_dijkstra_stake_pool_registration_certificate_missing_bls_key =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    operatorVerificationKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/node-pool/operator.vkey"
    vrfVerificationKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/node-pool/vrf.vkey"
    ownerVerificationKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/node-pool/owner.vkey"
    registrationCertFile <- noteTempFile tempDir "registration.cert"

    (exitCode, _stdout, stderr) <-
      execDetailCardanoCLI
        [ "dijkstra"
        , "stake-pool"
        , "registration-certificate"
        , "--testnet-magic"
        , "42"
        , "--pool-pledge"
        , "0"
        , "--pool-cost"
        , "0"
        , "--pool-margin"
        , "0"
        , "--cold-verification-key-file"
        , operatorVerificationKeyFile
        , "--vrf-verification-key-file"
        , vrfVerificationKeyFile
        , "--pool-reward-account-verification-key-file"
        , ownerVerificationKeyFile
        , "--pool-owner-stake-verification-key-file"
        , ownerVerificationKeyFile
        , "--out-file"
        , registrationCertFile
        ]

    exitCode H.=== ExitFailure 1
    H.assertWith stderr ("Missing: --bls-signing-key-file" `isInfixOf`)

-- | A pool cannot register a BLS voting key before Dijkstra, so the option is
-- not offered in Conway at all.
hprop_golden_conway_stake_pool_registration_certificate_rejects_bls_key :: Property
hprop_golden_conway_stake_pool_registration_certificate_rejects_bls_key =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    operatorVerificationKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/node-pool/operator.vkey"
    vrfVerificationKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/node-pool/vrf.vkey"
    ownerVerificationKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/node-pool/owner.vkey"
    blsSigningKeyFile <-
      noteInputFile "test/cardano-cli-golden/files/input/shelley/keys/bls_keys/signing_key"
    registrationCertFile <- noteTempFile tempDir "registration.cert"

    (exitCode, _stdout, stderr) <-
      execDetailCardanoCLI
        [ "conway"
        , "stake-pool"
        , "registration-certificate"
        , "--testnet-magic"
        , "42"
        , "--pool-pledge"
        , "0"
        , "--pool-cost"
        , "0"
        , "--pool-margin"
        , "0"
        , "--cold-verification-key-file"
        , operatorVerificationKeyFile
        , "--vrf-verification-key-file"
        , vrfVerificationKeyFile
        , "--bls-signing-key-file"
        , blsSigningKeyFile
        , "--pool-reward-account-verification-key-file"
        , ownerVerificationKeyFile
        , "--pool-owner-stake-verification-key-file"
        , ownerVerificationKeyFile
        , "--out-file"
        , registrationCertFile
        ]

    exitCode H.=== ExitFailure 1
    H.assertWith stderr ("Invalid option `--bls-signing-key-file'" `isInfixOf`)
