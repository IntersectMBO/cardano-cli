{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Key.NonExtendedKey where

import           Control.Monad (void)
import           System.FilePath ((</>))

import qualified Test.Cardano.CLI.Util as H
import           Test.Cardano.CLI.Util (execCardanoCLI, propertyOnce)

import           Hedgehog (Property)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Golden as H

{- HLINT ignore "Use camelCase" -}

-- | Test that converting a @cardano-address@ Byron signing key yields the
-- expected result.
hprop_golden_KeyNonExtendedKey_GenesisExtendedVerificationKey :: Property
hprop_golden_KeyNonExtendedKey_GenesisExtendedVerificationKey =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    genesisVKeyFp <- H.noteInputFile "test/cardano-cli-golden/files/input/key/non-extended-keys/shelley.000.vkey"
    nonExtendedFp <-  H.note "test/cardano-cli-golden/files/golden/key/non-extended-keys/non-extended-shelley.000.vkey"
    outFp <- H.note $ tempDir </> "non-extended-shelley.000.vkey"

    H.assertFilesExist [genesisVKeyFp]

    -- Convert the `cardano-address` signing key
    void $ execCardanoCLI
      [ "key", "non-extended-key"
      , "--extended-verification-key-file", genesisVKeyFp
      , "--verification-key-file", outFp
      ]

    H.diffFileVsGoldenFile outFp nonExtendedFp

-- | Test that converting a @cardano-address@ Byron signing key yields the
-- expected result.
hprop_golden_KeyNonExtendedKey_StakeExtendedVerificationKeyShelley :: Property
hprop_golden_KeyNonExtendedKey_StakeExtendedVerificationKeyShelley =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    genesisVKeyFp <- H.noteInputFile "test/cardano-cli-golden/files/input/key/non-extended-keys/stake.000.vkey"
    nonExtendedFp <-  H.note "test/cardano-cli-golden/files/golden/key/non-extended-keys/non-extended-stake.000.vkey"
    outFp <- H.note $ tempDir </> "non-extended-stake.000.vkey"

    H.assertFilesExist [genesisVKeyFp]

    -- Convert the `cardano-address` signing key
    void $ execCardanoCLI
      [ "key", "non-extended-key"
      , "--extended-verification-key-file", genesisVKeyFp
      , "--verification-key-file", outFp
      ]

    H.diffFileVsGoldenFile outFp nonExtendedFp

-- | Test that converting a drep extended verification key yields the
-- expected result.
hprop_golden_KeyNonExtendedKey_DRepExtendedVerificationKey :: Property
hprop_golden_KeyNonExtendedKey_DRepExtendedVerificationKey =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    extendedKeyFile <- H.noteInputFile "test/cardano-cli-golden/files/input/key/non-extended-keys/extended-drep.vkey"
    goldenFile <-  H.note "test/cardano-cli-golden/files/golden/key/non-extended-keys/non-extended-drep.vkey"
    outFp <- H.note $ tempDir </> "non-extended-drep.vkey"

    H.assertFilesExist [extendedKeyFile]

    void $ execCardanoCLI
      [ "conway", "key", "non-extended-key"
      , "--extended-verification-key-file", extendedKeyFile
      , "--verification-key-file", outFp
      ]

    H.diffFileVsGoldenFile outFp goldenFile

-- | Test that converting a payment extended verification key yields the
-- expected result.
hprop_golden_extended_payment_vkey_to_non_extended_vkey :: Property
hprop_golden_extended_payment_vkey_to_non_extended_vkey =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    extendedKeyFile <- H.noteInputFile "test/cardano-cli-golden/files/input/key/non-extended-keys/extended-payment.vkey"
    goldenFile <-  H.note "test/cardano-cli-golden/files/golden/key/non-extended-keys/non-extended-payment.vkey"
    outFp <- H.note $ tempDir </> "non-extended-payment.vkey"

    H.assertFilesExist [extendedKeyFile]

    void $ execCardanoCLI
      [ "conway", "key", "non-extended-key"
      , "--extended-verification-key-file", extendedKeyFile
      , "--verification-key-file", outFp
      ]

    H.diffFileVsGoldenFile outFp goldenFile
