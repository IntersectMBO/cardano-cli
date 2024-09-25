{- HLINT ignore "Use camelCase" -}

module Test.Cli.Governance.DRep where

import           Control.Monad

import           Test.Cardano.CLI.Util (execCardanoCLI, propertyOnce)

import           Hedgehog
import qualified Hedgehog.Extras.Test.Base as H

metadataUrls :: [String]
metadataUrls =
  [ "dummy-url"
  , "length-84-here-we-goooooooooooooooooooooooooooooooooooooooooooooooooooooo-dummy-url"
  , "exactly-128-chars-here-we-goooooooooooooooooooooooooooooooooooooooooooooooooooooo-dummy-url-ooooooooooooooooooooooooooooooooooo"
  ]

-- | This is a test of https://github.com/IntersectMBO/cardano-cli/issues/552
-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/governance drep registration certificate script hash/"'@
hprop_governance_drep_registration_certificate_script_hash :: Property
hprop_governance_drep_registration_certificate_script_hash =
  propertyOnce $ forM_ metadataUrls $ \metadataUrl -> do
    H.moduleWorkspace "tmp" $ \tempDir -> do
      outFile <- H.noteTempFile tempDir "drep-reg-cert.txt"

      H.noteShowM_ $
        execCardanoCLI
          [ "conway"
          , "governance"
          , "drep"
          , "registration-certificate"
          , "--drep-script-hash"
          , "00000000000000000000000000000000000000000000000000000003"
          , "--key-reg-deposit-amt"
          , "0"
          , "--drep-metadata-url"
          , metadataUrl
          , "--drep-metadata-hash"
          , "52e69500a92d80f2126c836a4903dc582006709f004cf7a28ed648f732dff8d2"
          , "--trust-drep-metadata-hash"
          , "--out-file"
          , outFile
          ]

-- | This is a test of https://github.com/IntersectMBO/cardano-cli/issues/552
-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/governance drep update certificate vkey file/"'@
hprop_governance_drep_update_certificate_vkey_file :: Property
hprop_governance_drep_update_certificate_vkey_file =
  propertyOnce $ forM_ metadataUrls $ \metadataUrl -> do
    H.moduleWorkspace "tmp" $ \tempDir -> do
      drepVKeyFile <- H.noteTempFile tempDir "drep.vkey"
      drepSKeyFile <- H.noteTempFile tempDir "drep.skey"

      H.noteShowM_ $
        execCardanoCLI
          [ "conway"
          , "governance"
          , "drep"
          , "key-gen"
          , "--verification-key-file"
          , drepVKeyFile
          , "--signing-key-file"
          , drepSKeyFile
          ]

      outFile <- H.noteTempFile tempDir "drep-upd-cert.txt"

      H.noteShowM_ $
        execCardanoCLI
          [ "conway"
          , "governance"
          , "drep"
          , "update-certificate"
          , "--drep-verification-key-file"
          , drepVKeyFile
          , "--drep-metadata-url"
          , metadataUrl
          , "--drep-metadata-hash"
          , "52e69500a92d80f2126c836a4903dc582006709f004cf7a28ed648f732dff8d2"
          , "--trust-drep-metadata-hash"
          , "--out-file"
          , outFile
          ]
