{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Cardano.Api.Polysemy
  ( File(..),
    FileDirection(..),
    VRFPrivateKeyFilePermissionError(..),
    ByronKey,
    ByronKeyLegacy,

    -- * Data family instances
    AsType(..),
    VerificationKey(..),
    SigningKey(..),
    Hash,

    -- * Legacy format
    IsByronKey(..),
    ByronKeyFormat(..),

    SomeByronSigningKey(..),

    SerialiseAsRawBytesError,

    checkVrfFilePermissions,
    deserialiseFromRawBytes,
    generateSigningKey,
    serialiseToRawBytes,
  ) where

import           Cardano.Api (AsType, File (..), FileDirection (..), Hash, Key,
                   SerialiseAsRawBytesError (..), SigningKey, VerificationKey (..),
                   deserialiseFromRawBytes, serialiseToRawBytes)
import qualified Cardano.Api as Api
import qualified Cardano.Api.IO as Api
import           Cardano.Api.IO.Base (VRFPrivateKeyFilePermissionError (..))
import           Cardano.Api.Keys.Byron (ByronKey, ByronKeyFormat (..), ByronKeyLegacy,
                   IsByronKey (..), SomeByronSigningKey (..))

import           Control.Monad.Except (runExceptT)

import           HaskellWorks.Polysemy
import           HaskellWorks.Prelude
import           Polysemy ()

generateSigningKey :: ()
  => Member (Embed IO) r
  => Key keyrole
  => AsType keyrole
  -> Sem r (SigningKey keyrole)
generateSigningKey keytype =
  embed (Api.generateSigningKey keytype)

checkVrfFilePermissions :: ()
  => Member (Error VRFPrivateKeyFilePermissionError) r
  => Member (Embed IO) r
  => File () In
  -> Sem r ()
checkVrfFilePermissions vrfSignKey =
  embed (runExceptT (Api.checkVrfFilePermissions vrfSignKey))
    & onLeftM throw
