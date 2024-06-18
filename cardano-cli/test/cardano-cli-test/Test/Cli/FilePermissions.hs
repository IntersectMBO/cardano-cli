{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cli.FilePermissions
  ( hprop_createVRFSigningKeyFilePermissions
  ) where

import           Test.Cardano.Api.Polysemy
import           Test.Cardano.CLI.Polysemy

import           HaskellWorks.Polysemy.Hedgehog
import           HaskellWorks.Prelude

-- | This property ensures that the VRF signing key file is created only with owner permissions
hprop_createVRFSigningKeyFilePermissions :: Property
hprop_createVRFSigningKeyFilePermissions = propertyOnce $ localWorkspace $ do
  -- Key filepaths
  vrfVerKey <- jotTempFile "VRF-verification-key-file"
  vrfSignKey <- jotTempFile "VRF-signing-key-file"

  -- Create VRF key pair
  execCardanoCli_
    [ "node", "key-gen-VRF"
    , "--verification-key-file", vrfVerKey
    , "--signing-key-file", vrfSignKey
    ]

  checkVrfFilePermissions (File vrfSignKey)
    -- key-gen-VRF cli command created a VRF signing key file with the wrong permissions
    & trapFail @VRFPrivateKeyFilePermissionError
