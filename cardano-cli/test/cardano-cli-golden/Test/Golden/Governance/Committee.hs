{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use camelCase" -}

module Test.Golden.Governance.Committee where

import Control.Monad (forM_, void)
import Data.Monoid (Last (..))
import System.Environment qualified as IO
import System.Exit (ExitCode (..))
import System.FilePath ((</>))

import Test.Cardano.CLI.Aeson (assertHasMappings)
import Test.Cardano.CLI.Hash
  ( exampleAnchorDataHash
  , exampleAnchorDataIpfsHash
  , exampleAnchorDataPathGolden
  , serveFilesWhile
  , tamperBase16Hash
  )
import Test.Cardano.CLI.Util
import Test.Cardano.CLI.Util qualified as H hiding (noteTempFile)

import Hedgehog (Property)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Hedgehog.Internal.Property ((===))

goldenDir, inputDir :: FilePath
goldenDir = "test/cardano-cli-golden/files/golden"
inputDir = "test/cardano-cli-golden/files/input"

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance committee key gen/"'@
hprop_golden_governance_committee_key_gen :: Property
hprop_golden_governance_committee_key_gen =
  let supplyValues =
        [ ("key-gen-cold", "Cold")
        , ("key-gen-hot", "Hot")
        ]
   in watchdogProp . propertyOnce $ forM_ supplyValues $ \(flag, inJson) ->
        H.moduleWorkspace "tmp" $ \tempDir -> do
          verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
          signingKeyFile <- noteTempFile tempDir "key-gen.skey"

          H.noteShowM_ $
            execCardanoCLI
              [ "conway"
              , "governance"
              , "committee"
              , flag
              , "--verification-key-file"
              , verificationKeyFile
              , "--signing-key-file"
              , signingKeyFile
              ]

          assertHasMappings
            [ ("type", "ConstitutionalCommittee" <> inJson <> "VerificationKey_ed25519")
            , ("description", "Constitutional Committee " <> inJson <> " Verification Key")
            ]
            verificationKeyFile

          assertHasMappings
            [ ("type", "ConstitutionalCommittee" <> inJson <> "SigningKey_ed25519")
            , ("description", "Constitutional Committee " <> inJson <> " Signing Key")
            ]
            signingKeyFile

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance CommitteeCreateHotKeyAuthorizationCertificate/"'@
hprop_golden_governance_CommitteeCreateHotKeyAuthorizationCertificate :: Property
hprop_golden_governance_CommitteeCreateHotKeyAuthorizationCertificate =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    ccColdVKey <- noteTempFile tempDir "cc-cold.vkey"
    ccColdSKey <- noteTempFile tempDir "cc-cold.skey"
    ccHotVKey <- noteTempFile tempDir "cc-hot.vkey"
    ccHotSKey <- noteTempFile tempDir "cc-hot.skey"

    certFile <- noteTempFile tempDir "hot-auth.cert"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "committee"
        , "key-gen-cold"
        , "--verification-key-file"
        , ccColdVKey
        , "--signing-key-file"
        , ccColdSKey
        ]

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "committee"
        , "key-gen-hot"
        , "--verification-key-file"
        , ccHotVKey
        , "--signing-key-file"
        , ccHotSKey
        ]

    H.noteShowM_ $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "committee"
        , "create-hot-key-authorization-certificate"
        , "--cold-verification-key-file"
        , ccColdVKey
        , "--hot-verification-key-file"
        , ccHotVKey
        , "--out-file"
        , certFile
        ]

    assertHasMappings
      [ ("type", "CertificateConway")
      , ("description", "Constitutional Committee Hot Key Registration Certificate")
      ]
      certFile

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance CommitteeCreateColdKeyResignationCertificate/"'@
hprop_golden_governance_CommitteeCreateColdKeyResignationCertificate :: Property
hprop_golden_governance_CommitteeCreateColdKeyResignationCertificate =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    ccColdVKey <- noteTempFile tempDir "cold.vkey"
    ccColdSKey <- noteTempFile tempDir "cold.skey"

    certFile <- noteTempFile tempDir "hot-auth.cert"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "committee"
        , "key-gen-cold"
        , "--verification-key-file"
        , ccColdVKey
        , "--signing-key-file"
        , ccColdSKey
        ]

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "committee"
        , "create-cold-key-resignation-certificate"
        , "--cold-verification-key-file"
        , ccColdVKey
        , "--out-file"
        , certFile
        ]

    assertHasMappings
      [ ("type", "CertificateConway")
      , ("description", "Constitutional Committee Cold Key Resignation Certificate")
      ]
      certFile

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance UpdateCommittee/"'@
hprop_golden_governance_UpdateCommittee :: Property
hprop_golden_governance_UpdateCommittee =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    stakeVkey <- noteInputFile $ inputDir </> "governance/stake-address.vkey"
    ccProposal <- noteInputFile $ inputDir </> "governance/committee/cc-proposal.txt"
    coldCCVkey1 <- noteInputFile $ inputDir </> "governance/committee/cc-cold1.vkey"
    coldCCVkey2 <- noteInputFile $ inputDir </> "governance/committee/cc-cold2.vkey"

    outFile <- H.noteTempFile tempDir "answer-file.json"

    proposalHash <-
      execCardanoCLI
        [ "hash"
        , "anchor-data"
        , "--file-text"
        , ccProposal
        ]

    H.note_ proposalHash
    H.note_ $ show $ length proposalHash

    goldenAnswerFile <- H.note $ goldenDir </> "governance/committee/update-committee-answer.json"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "action"
        , "update-committee"
        , "--testnet"
        , "--governance-action-deposit"
        , "0"
        , "--deposit-return-stake-verification-key-file"
        , stakeVkey
        , "--anchor-url"
        , "http://dummy"
        , "--anchor-data-hash"
        , proposalHash
        , "--add-cc-cold-verification-key-file"
        , coldCCVkey1
        , "--epoch"
        , "202"
        , "--add-cc-cold-verification-key-file"
        , coldCCVkey2
        , "--epoch"
        , "252"
        , "--threshold"
        , "51/100"
        , "--out-file"
        , outFile
        ]

    H.diffFileVsGoldenFile outFile goldenAnswerFile

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance committee cold extended key signing/"'@
hprop_golden_governance_committee_cold_extended_key_signing :: Property
hprop_golden_governance_committee_cold_extended_key_signing =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    skeyFile <- noteInputFile $ inputDir </> "governance/committee/cc.extended.cold.skey"
    txBody <- noteInputFile $ inputDir </> "governance/drep/extended-key-signing/tx.body"

    outGold <- H.note $ goldenDir </> "governance/committee/tx.cold.extended.signed"
    outFile <- H.noteTempFile tempDir "outFile"

    H.noteM_ $
      execCardanoCLI
        [ "conway"
        , "transaction"
        , "sign"
        , "--tx-body-file"
        , txBody
        , "--signing-key-file"
        , skeyFile
        , "--out-file"
        , outFile
        ]

    H.diffFileVsGoldenFile outFile outGold

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance committee hot extended key signing/"'@
hprop_golden_governance_committee_hot_extended_key_signing :: Property
hprop_golden_governance_committee_hot_extended_key_signing =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    skeyFile <- noteInputFile $ inputDir </> "governance/committee/cc.extended.hot.skey"
    txBody <- noteInputFile $ inputDir </> "governance/drep/extended-key-signing/tx.body"

    outGold <- H.note $ goldenDir </> "governance/committee/tx.hot.extended.signed"
    outFile <- H.noteTempFile tempDir "outFile"

    H.noteM_ $
      execCardanoCLI
        [ "conway"
        , "transaction"
        , "sign"
        , "--tx-body-file"
        , txBody
        , "--signing-key-file"
        , skeyFile
        , "--out-file"
        , outFile
        ]

    H.diffFileVsGoldenFile outFile outGold

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden verification key committee/"'@
hprop_golden_verification_key_committee :: Property
hprop_golden_verification_key_committee = do
  let values =
        [
          ( inputDir </> "governance/committee/cc.extended.hot.skey"
          , goldenDir </> "governance/committee/cc.extended.hot.vkey"
          )
        ,
          ( inputDir </> "governance/committee/cc.extended.cold.skey"
          , goldenDir </> "governance/committee/cc.extended.cold.vkey"
          )
        ]

  watchdogProp . propertyOnce $ forM_ values $ \(skeyFile, vkeyGolden) ->
    H.moduleWorkspace "tmp" $ \tempDir -> do
      vkeyFileOut <- noteTempFile tempDir "cc.extended.vkey"

      H.noteM_ $
        execCardanoCLI
          [ "conway"
          , "key"
          , "verification-key"
          , "--signing-key-file"
          , skeyFile
          , "--verification-key-file"
          , vkeyFileOut
          ]

      H.diffFileVsGoldenFile vkeyFileOut vkeyGolden

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance extended committee key hash/"'@
hprop_golden_governance_extended_committee_key_hash :: Property
hprop_golden_governance_extended_committee_key_hash =
  let supplyValues =
        [
          ( inputDir </> "governance/committee/cc.extended.cold.vkey"
          , "9fe92405abcd903d34e21a97328e7cd222eebd4ced5995a95777f7a3\n"
          )
        ,
          ( inputDir </> "governance/committee/cc.extended.hot.vkey"
          , "4eb7202ffcc6d5513dba5edc618bd7b582a257c76d6b0cd83975f4e6\n"
          )
        ]
   in watchdogProp . propertyOnce $ forM_ supplyValues $ \(extendedKeyFile, expected) -> do
        verificationKeyFile <- H.noteInputFile extendedKeyFile

        result <-
          execCardanoCLI
            [ "conway"
            , "governance"
            , "committee"
            , "key-hash"
            , "--verification-key-file"
            , verificationKeyFile
            ]

        result H.=== expected

-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/golden governance committee checks wrong hash fails/"'@
hprop_golden_governance_committee_checks_wrong_hash_fails :: Property
hprop_golden_governance_committee_checks_wrong_hash_fails =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- We modify the hash slightly so that the hash check fails
    alteredHash <- H.evalMaybe $ tamperBase16Hash exampleAnchorDataHash
    let relativeUrl = ["ipfs", exampleAnchorDataIpfsHash]

    ccColdVKey <- noteTempFile tempDir "cold.vkey"
    ccColdSKey <- noteTempFile tempDir "cold.skey"

    certFile <- noteTempFile tempDir "hot-auth.cert"

    void $
      execCardanoCLI
        [ "conway"
        , "governance"
        , "committee"
        , "key-gen-cold"
        , "--verification-key-file"
        , ccColdVKey
        , "--signing-key-file"
        , ccColdSKey
        ]

    -- Create temporary HTTP server with files required by the call to `cardano-cli`
    env <- H.evalIO IO.getEnvironment
    (exitCode, _, result) <-
      serveFilesWhile
        [ (relativeUrl, exampleAnchorDataPathGolden)
        ]
        ( \port -> do
            execDetailConfigCardanoCLI
              ( H.defaultExecConfig
                  { H.execConfigEnv = Last $ Just (("IPFS_GATEWAY_URI", "http://localhost:" ++ show port ++ "/") : env)
                  }
              )
              [ "conway"
              , "governance"
              , "committee"
              , "create-cold-key-resignation-certificate"
              , "--cold-verification-key-file"
              , ccColdVKey
              , "--resignation-metadata-url"
              , "ipfs://" ++ exampleAnchorDataIpfsHash
              , "--resignation-metadata-hash"
              , alteredHash
              , "--check-resignation-metadata-hash"
              , "--out-file"
              , certFile
              ]
        )

    exitCode === ExitFailure 1

    H.diffVsGoldenFileExcludeTrace
      result
      "test/cardano-cli-golden/files/golden/governance/committee/governance_committee_checks_wrong_hash_fails.out"
