{-# LANGUAGE OverloadedStrings #-}

module Test.Cli.FilePermissions
  ( hprop_createVRFSigningKeyFilePermissions
  )
where

import Cardano.Api
import Cardano.Api.Internal.IO (checkVrfFilePermissions)

import Control.Monad (void)

import Test.Cardano.CLI.Util (execCardanoCLI)

import Hedgehog (Property, success)
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Internal.Property (failWith)

-- | This property ensures that the VRF signing key file is created only with owner permissions
hprop_createVRFSigningKeyFilePermissions :: Property
hprop_createVRFSigningKeyFilePermissions =
  H.propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- Key filepaths
    vrfVerKey <- H.noteTempFile tempDir "VRF-verification-key-file"

    vrfSignKey <- H.noteTempFile tempDir "VRF-signing-key-file"

    -- Create VRF key pair
    void $
      execCardanoCLI
        [ "latest"
        , "node"
        , "key-gen-VRF"
        , "--verification-key-file"
        , vrfVerKey
        , "--signing-key-file"
        , vrfSignKey
        ]

    result <- liftIO . runExceptT $ checkVrfFilePermissions (File vrfSignKey)
    case result of
      Left err ->
        failWith Nothing $
          "key-gen-VRF cli command created a VRF signing key \
          \file with the wrong permissions: "
            <> show err
      Right () -> success
