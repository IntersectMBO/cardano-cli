{- HLINT ignore "Use camelCase" -}

module Test.Golden.Governance.DRep where

import           Control.Monad (void)

import           Test.Cardano.CLI.Util (execCardanoCLI, noteInputFile, propertyOnce)

import           Hedgehog
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Golden as H

hprop_golden_governanceDRepKeyGen :: Property
hprop_golden_governanceDRepKeyGen =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    verificationKeyFile <- H.noteTempFile tempDir "key-gen.vkey"
    signingKeyFile <- H.noteTempFile tempDir "key-gen.skey"

    void $ execCardanoCLI
      [  "conway", "governance", "drep", "key-gen"
      , "--verification-key-file", verificationKeyFile
      , "--signing-key-file", signingKeyFile
      ]

    H.assertFileOccurences 1 "DRepVerificationKey_ed25519" verificationKeyFile
    H.assertFileOccurences 1 "DRepSigningKey_ed25519" signingKeyFile

    H.assertFileOccurences 1 "Delegate Representative Verification Key" verificationKeyFile
    H.assertFileOccurences 1 "Delegate Representative Signing Key" signingKeyFile

hprop_golden_governance_drep_id_bech32 :: Property
hprop_golden_governance_drep_id_bech32 =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    vkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/drep.vkey"
    idFile <- H.noteTempFile tempDir "drep.id.bech32"
    idGold <- H.note "test/cardano-cli-golden/files/golden/governance/drep/drep.id.bech32"

    void $ execCardanoCLI
      [  "conway", "governance", "drep", "id"
      , "--drep-verification-key-file", vkeyFile
      , "--output-format", "bech32"
      , "--out-file", idFile
      ]

    H.diffFileVsGoldenFile idFile idGold

hprop_golden_governance_drep_id_hex :: Property
hprop_golden_governance_drep_id_hex =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    vkeyFile <- noteInputFile "test/cardano-cli-golden/files/input/drep.vkey"
    idFile <- H.noteTempFile tempDir "drep.id.hex"
    idGold <- H.note "test/cardano-cli-golden/files/golden/governance/drep/drep.id.hex"

    void $ execCardanoCLI
      [  "conway", "governance", "drep", "id"
      , "--drep-verification-key-file", vkeyFile
      , "--output-format", "hex"
      , "--out-file", idFile
      ]

    H.diffFileVsGoldenFile idFile idGold
