{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Byron.Key
  ( -- * Keys
    ByronKeyFailure (..)
  , NewSigningKeyFile (..)
  , NewVerificationKeyFile (..)
  , VerificationKeyFile
  , prettyPublicKey
  , readByronSigningKey
  , readPaymentVerificationKey
  , renderByronKeyFailure
  , byronWitnessToVerKey
  )
where

import Cardano.Api.Byron
import Cardano.Api.Key
import Cardano.Api.Monad.Error
import Cardano.Api.Pretty
import Cardano.Api.Serialise.Raw

import Cardano.CLI.Type.Common
import Cardano.Crypto.Signing qualified as Crypto

import Control.Exception (Exception (..))
import Data.ByteString qualified as SB
import Data.ByteString.UTF8 qualified as UTF8
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Formatting (build, sformat, (%))

data ByronKeyFailure
  = ReadSigningKeyFailure !FilePath !Text
  | ReadVerificationKeyFailure !FilePath !Text
  | LegacySigningKeyDeserialisationFailed !FilePath
  | SigningKeyDeserialisationFailed !FilePath
  | VerificationKeyDeserialisationFailed !FilePath !Text
  | CannotMigrateFromNonLegacySigningKey !FilePath
  deriving Show

instance Error ByronKeyFailure where
  prettyError = renderByronKeyFailure

renderByronKeyFailure :: ByronKeyFailure -> Doc ann
renderByronKeyFailure = \case
  CannotMigrateFromNonLegacySigningKey fp ->
    "Migrate from non-legacy Byron key unnecessary: " <> pshow fp
  ReadSigningKeyFailure sKeyFp readErr ->
    "Error reading signing key at: " <> pshow sKeyFp <> " Error: " <> pshow readErr
  ReadVerificationKeyFailure vKeyFp readErr ->
    "Error reading verification key at: " <> pshow vKeyFp <> " Error: " <> pshow readErr
  LegacySigningKeyDeserialisationFailed fp ->
    "Error attempting to deserialise a legacy signing key at: " <> pshow fp
  SigningKeyDeserialisationFailed sKeyFp ->
    "Error deserialising signing key at: " <> pshow sKeyFp
  VerificationKeyDeserialisationFailed vKeyFp deSerError ->
    "Error deserialising verification key at: " <> pshow vKeyFp <> " Error: " <> pshow deSerError

newtype NewSigningKeyFile
  = NewSigningKeyFile FilePath
  deriving (Eq, Ord, Show, IsString)

newtype NewVerificationKeyFile
  = NewVerificationKeyFile FilePath
  deriving (Eq, Ord, Show, IsString)

-- | Print some invariant properties of a public key:
--   its hash and formatted view.
prettyPublicKey :: VerificationKey ByronKey -> Text
prettyPublicKey (ByronVerificationKey vk) =
  sformat
    ( "    public key hash: "
        % build
        % "\npublic key (base64): "
        % Crypto.fullVerificationKeyF
        % "\n   public key (hex): "
        % Crypto.fullVerificationKeyHexF
    )
    (addressHash vk)
    vk
    vk

byronWitnessToVerKey :: SomeByronSigningKey -> VerificationKey ByronKey
byronWitnessToVerKey (AByronSigningKeyLegacy sKeyLeg) = castVerificationKey $ getVerificationKey sKeyLeg
byronWitnessToVerKey (AByronSigningKey sKeyNonLeg) = getVerificationKey sKeyNonLeg

-- TODO:  we need to support password-protected secrets.

-- | Read signing key from a file.
readByronSigningKey
  :: ByronKeyFormat -> SigningKeyFile In -> ExceptT ByronKeyFailure IO SomeByronSigningKey
readByronSigningKey bKeyFormat (File fp) = do
  sK <- handleIOExceptT (ReadSigningKeyFailure fp . T.pack . displayException) $ SB.readFile fp
  case bKeyFormat of
    LegacyByronKeyFormat ->
      case deserialiseFromRawBytes (AsSigningKey AsByronKeyLegacy) sK of
        Right legKey -> right $ AByronSigningKeyLegacy legKey
        Left _ -> left $ LegacySigningKeyDeserialisationFailed fp
    NonLegacyByronKeyFormat ->
      case deserialiseFromRawBytes (AsSigningKey AsByronKey) sK of
        Right nonLegSKey -> right $ AByronSigningKey nonLegSKey
        Left _ -> left $ SigningKeyDeserialisationFailed fp

-- | Read verification key from a file.  Throw an error if the file can't be read
-- or the key fails to deserialise.
readPaymentVerificationKey
  :: VerificationKeyFile In -> ExceptT ByronKeyFailure IO Crypto.VerificationKey
readPaymentVerificationKey (File fp) = do
  vkB <- handleIOExceptT (ReadVerificationKeyFailure fp . T.pack . displayException) (SB.readFile fp)
  -- Verification Key
  let eVk = hoistEither . Crypto.parseFullVerificationKey . fromString $ UTF8.toString vkB
  -- Convert error to 'CliError'
  firstExceptT (VerificationKeyDeserialisationFailed fp . T.pack . show) eVk
