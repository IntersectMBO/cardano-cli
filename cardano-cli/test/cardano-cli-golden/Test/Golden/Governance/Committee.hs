{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use camelCase" -}

module Test.Golden.Governance.Committee where

import           Control.Monad (forM_, void)
import           System.FilePath ((</>))

import           Test.Cardano.CLI.Aeson (assertHasMappings)
import qualified Test.Cardano.CLI.Util as H hiding (noteTempFile)
import           Test.Cardano.CLI.Util

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Golden as H

goldenDir, inputDir :: FilePath
goldenDir = "test/cardano-cli-golden/files/golden"
inputDir  = "test/cardano-cli-golden/files/input"

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance committee key gen/"'@
hprop_golden_governance_committee_key_gen :: Property
hprop_golden_governance_committee_key_gen =
  let supplyValues = [ ("key-gen-cold", "Cold")
                     , ("key-gen-hot",  "Hot") ] in
  propertyOnce $ forM_ supplyValues $ \(flag, inJson) ->
    H.moduleWorkspace "tmp" $ \tempDir -> do
    verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
    signingKeyFile <- noteTempFile tempDir "key-gen.skey"

    H.noteShowM_ $ execCardanoCLI
      [ "conway", "governance", "committee", flag
      , "--verification-key-file", verificationKeyFile
      , "--signing-key-file", signingKeyFile
      ]

    assertHasMappings [ ("type",        "ConstitutionalCommittee" <> inJson <> "VerificationKey_ed25519")
                      , ("description", "Constitutional Committee " <> inJson <> " Verification Key")]
                      verificationKeyFile

    assertHasMappings [ ("type",        "ConstitutionalCommittee" <> inJson <> "SigningKey_ed25519")
                      , ("description", "Constitutional Committee " <> inJson <> " Signing Key")]
                      signingKeyFile

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance CommitteeCreateHotKeyAuthorizationCertificate/"'@
hprop_golden_governance_CommitteeCreateHotKeyAuthorizationCertificate :: Property
hprop_golden_governance_CommitteeCreateHotKeyAuthorizationCertificate =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    ccColdVKey <- noteTempFile tempDir "cc-cold.vkey"
    ccColdSKey <- noteTempFile tempDir "cc-cold.skey"
    ccHotVKey <- noteTempFile tempDir "cc-hot.vkey"
    ccHotSKey <- noteTempFile tempDir "cc-hot.skey"

    certFile <- noteTempFile tempDir "hot-auth.cert"

    void $ execCardanoCLI
      [ "conway", "governance", "committee", "key-gen-cold"
      , "--verification-key-file", ccColdVKey
      , "--signing-key-file", ccColdSKey
      ]

    void $ execCardanoCLI
      [ "conway", "governance", "committee", "key-gen-hot"
      , "--verification-key-file", ccHotVKey
      , "--signing-key-file", ccHotSKey
      ]

    H.noteShowM_ $ execCardanoCLI
      [ "conway", "governance", "committee", "create-hot-key-authorization-certificate"
      , "--cold-verification-key-file", ccColdVKey
      , "--hot-key-file", ccHotVKey
      , "--out-file", certFile
      ]

    assertHasMappings [ ("type", "CertificateConway")
                      , ("description", "Constitutional Committee Hot Key Registration Certificate")]
                      certFile

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance CommitteeCreateColdKeyResignationCertificate/"'@
hprop_golden_governance_CommitteeCreateColdKeyResignationCertificate :: Property
hprop_golden_governance_CommitteeCreateColdKeyResignationCertificate =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    ccColdVKey <- noteTempFile tempDir "cold.vkey"
    ccColdSKey <- noteTempFile tempDir "cold.skey"

    certFile <- noteTempFile tempDir "hot-auth.cert"

    void $ execCardanoCLI
      [ "conway", "governance", "committee", "key-gen-cold"
      , "--verification-key-file", ccColdVKey
      , "--signing-key-file", ccColdSKey
      ]

    void $ execCardanoCLI
      [  "conway", "governance", "committee", "create-cold-key-resignation-certificate"
      , "--cold-verification-key-file", ccColdVKey
      , "--out-file", certFile
      ]

    assertHasMappings [ ("type", "CertificateConway")
                      , ("description", "Constitutional Committee Cold Key Resignation Certificate")]
                      certFile

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance UpdateCommittee/"'@
hprop_golden_governance_UpdateCommittee :: Property
hprop_golden_governance_UpdateCommittee =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    stakeVkey <- noteInputFile $ inputDir </> "governance/stake-address.vkey"
    ccProposal <- noteInputFile $ inputDir </> "governance/committee/cc-proposal.txt"
    coldCCVkey1 <- noteInputFile $ inputDir </> "governance/committee/cc-cold1.vkey"
    coldCCVkey2 <- noteInputFile $ inputDir </> "governance/committee/cc-cold2.vkey"

    outFile <- H.noteTempFile tempDir "answer-file.json"

    proposalHash <- execCardanoCLI
      [ "conway", "governance", "hash", "anchor-data"
      , "--file-text", ccProposal ]

    H.note_ proposalHash
    H.note_ $ show $ length proposalHash

    goldenAnswerFile <- H.note $ goldenDir </> "governance/committee/update-committee-answer.json"

    void $ execCardanoCLI
      [ "conway", "governance", "action", "update-committee"
      , "--testnet", "--governance-action-deposit", "0"
      , "--deposit-return-stake-verification-key-file", stakeVkey
      , "--anchor-url", "http://dummy"
      , "--anchor-data-hash", proposalHash
      , "--add-cc-cold-verification-key-file", coldCCVkey1
      , "--epoch", "202"
      , "--add-cc-cold-verification-key-file", coldCCVkey2
      , "--epoch", "252"
      , "--threshold", "51/100"
      , "--out-file", outFile
      ]

    H.diffFileVsGoldenFile outFile goldenAnswerFile

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance committee cold extended key signing/"'@
hprop_golden_governance_committee_cold_extended_key_signing :: Property
hprop_golden_governance_committee_cold_extended_key_signing =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    skeyFile <- noteInputFile $ inputDir </> "governance/committee/cc.extended.cold.skey"
    txBody <- noteInputFile $ inputDir </> "governance/drep/extended-key-signing/tx.body"

    outGold <- H.note $ goldenDir </> "governance/committee/tx.cold.extended.signed"
    outFile <- H.noteTempFile tempDir "outFile"

    H.noteM_ $ execCardanoCLI
      [  "conway", "transaction", "sign"
      , "--tx-body-file", txBody
      , "--signing-key-file", skeyFile
      , "--out-file", outFile
      ]

    H.diffFileVsGoldenFile outFile outGold

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance committee hot extended key signing/"'@
hprop_golden_governance_committee_hot_extended_key_signing :: Property
hprop_golden_governance_committee_hot_extended_key_signing =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    skeyFile <- noteInputFile $ inputDir </> "governance/committee/cc.extended.hot.skey"
    txBody <- noteInputFile $ inputDir </> "governance/drep/extended-key-signing/tx.body"

    outGold <- H.note $ goldenDir </> "governance/committee/tx.hot.extended.signed"
    outFile <- H.noteTempFile tempDir "outFile"

    H.noteM_ $ execCardanoCLI
      [  "conway", "transaction", "sign"
      , "--tx-body-file", txBody
      , "--signing-key-file", skeyFile
      , "--out-file", outFile
      ]

    H.diffFileVsGoldenFile outFile outGold

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden verification key committee/"'@
hprop_golden_verification_key_committee :: Property
hprop_golden_verification_key_committee = do
  let values = [ ( inputDir </> "governance/committee/cc.extended.hot.skey"
                 , goldenDir </> "governance/committee/cc.extended.hot.vkey"
                 )
                 ,
                 ( inputDir </> "governance/committee/cc.extended.cold.skey"
                 , goldenDir </> "governance/committee/cc.extended.cold.vkey"
                 )
               ]

  propertyOnce $ forM_ values $ \(skeyFile, vkeyGolden) ->
    H.moduleWorkspace "tmp" $ \tempDir -> do
      vkeyFileOut <- noteTempFile tempDir "cc.extended.vkey"

      H.noteM_ $ execCardanoCLI
        [  "conway", "key", "verification-key"
        , "--signing-key-file", skeyFile
        , "--verification-key-file", vkeyFileOut
        ]

      H.diffFileVsGoldenFile vkeyFileOut vkeyGolden

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance extended committee key hash/"'@
hprop_golden_governance_extended_committee_key_hash :: Property
hprop_golden_governance_extended_committee_key_hash =
  let supplyValues = [ (inputDir </> "governance/committee/cc.extended.cold.vkey", "9fe92405abcd903d34e21a97328e7cd222eebd4ced5995a95777f7a3\n")
                     , (inputDir </> "governance/committee/cc.extended.hot.vkey",  "4eb7202ffcc6d5513dba5edc618bd7b582a257c76d6b0cd83975f4e6\n")
                     ] in
  propertyOnce $ forM_ supplyValues $ \(extendedKeyFile, expected) ->
    H.moduleWorkspace "tmp" $ \_tempDir -> do
    verificationKeyFile <- H.noteInputFile extendedKeyFile

    result <- execCardanoCLI
      [  "conway", "governance", "committee", "key-hash"
      , "--verification-key-file", verificationKeyFile
      ]

    result H.=== expected
