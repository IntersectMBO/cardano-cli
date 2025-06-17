{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use camelCase" -}

module Test.Golden.Governance.Committee where

import Control.Monad (void)
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import System.Environment qualified as IO
import System.Exit (ExitCode (..))
import System.FilePath ((</>))

import Test.Cardano.CLI.Aeson (redactJsonFieldsInFile)
import Test.Cardano.CLI.Hash
  ( exampleAnchorDataHash
  , exampleAnchorDataIpfsHash
  , exampleAnchorDataPathGolden
  , serveFilesWhile
  , tamperBase16Hash
  )
import Test.Cardano.CLI.Util
import Test.Cardano.CLI.Util qualified as H hiding (noteTempFile)

import Hedgehog qualified as H
import Hedgehog.Extras (UnitIO)
import Hedgehog.Extras qualified as H
import Hedgehog.Internal.Property ((===))
import Test.Golden.Util

inputDir :: FilePath
inputDir = "test/cardano-cli-golden/files/input"

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden_governance_committee_KeyGenCold/"'@
tasty_golden_governance_committee_KeyGenCold :: UnitIO ()
tasty_golden_governance_committee_KeyGenCold =
  H.moduleWorkspace "tmp" $ \tempDir -> do
    H.note_ tempDir
    ccColdVKey <- noteTempFile tempDir "cold.vkey"
    ccColdSKey <- noteTempFile tempDir "cold.skey"
    ccColdVKeyRedacted <- noteTempFile tempDir "cold.vkey.redacted"
    ccColdSKeyRedacted <- noteTempFile tempDir "cold.skey.redacted"
    ccColdVKeyRedactedGoldenFile <- noteGoldenFile "cold.vkey.redacted"
    ccColdSKeyRedactedGoldenFile <- noteGoldenFile "cold.skey.redacted"

    let redactions =
          Map.fromList
            [ ("cborHex", "<redacted>")
            ]

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

    redactJsonFieldsInFile redactions ccColdVKey ccColdVKeyRedacted
    redactJsonFieldsInFile redactions ccColdSKey ccColdSKeyRedacted

    H.diffFileVsGoldenFile ccColdVKeyRedacted ccColdVKeyRedactedGoldenFile
    H.diffFileVsGoldenFile ccColdSKeyRedacted ccColdSKeyRedactedGoldenFile

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden_governance_committee_KeyGenHot/"'@
tasty_golden_governance_committee_KeyGenHot :: UnitIO ()
tasty_golden_governance_committee_KeyGenHot =
  H.moduleWorkspace "tmp" $ \tempDir -> do
    H.note_ tempDir
    ccHotVKey <- noteTempFile tempDir "hot.vkey"
    ccHotSKey <- noteTempFile tempDir "hot.skey"
    ccHotVKeyRedacted <- noteTempFile tempDir "hot.vkey.redacted"
    ccHotSKeyRedacted <- noteTempFile tempDir "hot.skey.redacted"
    ccHotVKeyRedactedGoldenFile <- noteGoldenFile "hot.vkey.redacted"
    ccHotSKeyRedactedGoldenFile <- noteGoldenFile "hot.skey.redacted"

    let redactions =
          Map.fromList
            [ ("cborHex", "<redacted>")
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

    redactJsonFieldsInFile redactions ccHotVKey ccHotVKeyRedacted
    redactJsonFieldsInFile redactions ccHotSKey ccHotSKeyRedacted

    H.diffFileVsGoldenFile ccHotVKeyRedacted ccHotVKeyRedactedGoldenFile
    H.diffFileVsGoldenFile ccHotSKeyRedacted ccHotSKeyRedactedGoldenFile

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance CommitteeCreateHotKeyAuthorizationCertificate/"'@
tasty_golden_governance_CommitteeCreateHotKeyAuthorizationCertificate :: UnitIO ()
tasty_golden_governance_CommitteeCreateHotKeyAuthorizationCertificate =
  H.moduleWorkspace "tmp" $ \tempDir -> do
    ccColdVKey <- noteInputFile $ inputDir </> "governance/cc-cold.vkey"
    ccHotVKey <- noteInputFile $ inputDir </> "governance/cc-hot.vkey"

    certFile <- noteTempFile tempDir "hot-auth.cert"
    goldenCertFile <- noteGoldenFile "hot-key-authorization-certificate.cert"

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

    H.diffFileVsGoldenFile certFile goldenCertFile

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance CommitteeCreateColdKeyResignationCertificate/"'@
tasty_golden_governance_CommitteeCreateColdKeyResignationCertificate :: UnitIO ()
tasty_golden_governance_CommitteeCreateColdKeyResignationCertificate =
  H.moduleWorkspace "tmp" $ \tempDir -> do
    ccColdVKey <- noteInputFile $ inputDir </> "governance/cc-cold.vkey"
    certFile <- noteTempFile tempDir "hot-auth.cert"
    goldenCertFile <- noteGoldenFile "cold-key-resignation-certificate.cert"

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

    H.diffFileVsGoldenFile certFile goldenCertFile

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance UpdateCommittee/"'@
tasty_golden_governance_UpdateCommittee :: UnitIO ()
tasty_golden_governance_UpdateCommittee =
  H.moduleWorkspace "tmp" $ \tempDir -> do
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

    goldenAnswerFile <- noteGoldenFile "update-committee-answer.json"

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
tasty_golden_governance_committee_cold_extended_key_signing :: UnitIO ()
tasty_golden_governance_committee_cold_extended_key_signing =
  H.moduleWorkspace "tmp" $ \tempDir -> do
    skeyFile <- noteInputFile $ inputDir </> "governance/committee/cc.extended.cold.skey"
    txBody <- noteInputFile $ inputDir </> "governance/drep/extended-key-signing/tx.body"

    outFile <- H.noteTempFile tempDir "outFile"

    goldenFile <- noteGoldenFile "tx.cold.extended.signed"

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

    H.diffFileVsGoldenFile outFile goldenFile

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance committee hot extended key signing/"'@
tasty_golden_governance_committee_hot_extended_key_signing :: UnitIO ()
tasty_golden_governance_committee_hot_extended_key_signing =
  H.moduleWorkspace "tmp" $ \tempDir -> do
    skeyFile <- noteInputFile $ inputDir </> "governance/committee/cc.extended.hot.skey"
    txBody <- noteInputFile $ inputDir </> "governance/drep/extended-key-signing/tx.body"

    outFile <- H.noteTempFile tempDir "outFile"

    goldenFile <- noteGoldenFile "tx.hot.extended.signed"

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

    H.diffFileVsGoldenFile outFile goldenFile

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden hot verification key committee/"'@
tasty_golden_hot_verification_key_committee :: UnitIO ()
tasty_golden_hot_verification_key_committee =
  H.moduleWorkspace "tmp" $ \tempDir -> do
    skeyFile <- noteInputFile $ inputDir </> "governance/committee/cc.extended.hot.skey"
    goldenFile <- noteGoldenFile "cc.extended.hot.vkey"
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

    H.diffFileVsGoldenFile vkeyFileOut goldenFile

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden cold verification key committee/"'@
tasty_golden_cold_verification_key_committee :: UnitIO ()
tasty_golden_cold_verification_key_committee =
  H.moduleWorkspace "tmp" $ \tempDir -> do
    skeyFile <- noteInputFile $ inputDir </> "governance/committee/cc.extended.cold.skey"
    goldenFile <- noteGoldenFile "cc.extended.cold.vkey"
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

    H.diffFileVsGoldenFile vkeyFileOut goldenFile

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance extended hot committee key hash/"'@
tasty_golden_governance_extended_hot_committee_key_hash :: UnitIO ()
tasty_golden_governance_extended_hot_committee_key_hash = do
  extendedKeyFile <- noteInputFile $ inputDir </> "governance/committee/cc.extended.hot.vkey"
  verificationKeyFile <- H.noteInputFile extendedKeyFile
  goldenFile <- noteGoldenFile "cc.extended.hot.vkey.hash"

  result <-
    execCardanoCLI
      [ "conway"
      , "governance"
      , "committee"
      , "key-hash"
      , "--verification-key-file"
      , verificationKeyFile
      ]

  H.diffVsGoldenFile result goldenFile

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden governance extended cold committee key hash/"'@
tasty_golden_governance_extended_cold_committee_key_hash :: UnitIO ()
tasty_golden_governance_extended_cold_committee_key_hash = do
  extendedKeyFile <- noteInputFile $ inputDir </> "governance/committee/cc.extended.cold.vkey"
  verificationKeyFile <- noteInputFile extendedKeyFile
  goldenFile <- noteGoldenFile "cc.extended.cold.vkey.hash"

  result <-
    execCardanoCLI
      [ "conway"
      , "governance"
      , "committee"
      , "key-hash"
      , "--verification-key-file"
      , verificationKeyFile
      ]

  H.diffVsGoldenFile result goldenFile

-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/golden governance committee checks wrong hash fails/"'@
tasty_golden_governance_committee_checks_wrong_hash_fails :: UnitIO ()
tasty_golden_governance_committee_checks_wrong_hash_fails =
  H.moduleWorkspace "tmp" $ \tempDir -> do
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
