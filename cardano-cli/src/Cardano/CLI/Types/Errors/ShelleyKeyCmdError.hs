{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Types.Errors.ShelleyKeyCmdError
  ( ShelleyKeyCmdError(..)
  , renderShelleyKeyCmdError
  ) where

import           Cardano.Api

import qualified Cardano.CLI.Byron.Key as Byron
import           Cardano.CLI.Types.Errors.CardanoAddressSigningKeyConversionError
import           Cardano.CLI.Types.Errors.ItnKeyConversionError
import           Cardano.CLI.Types.Key

import           Data.Text (Text)
import qualified Data.Text as Text

data ShelleyKeyCmdError
  = ShelleyKeyCmdReadFileError !(FileError TextEnvelopeError)
  | ShelleyKeyCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyKeyCmdWriteFileError !(FileError ())
  | ShelleyKeyCmdByronKeyFailure !Byron.ByronKeyFailure
  | ShelleyKeyCmdByronKeyParseError
      !Text
      -- ^ Text representation of the parse error. Unfortunately, the actual
      -- error type isn't exported.
  | ShelleyKeyCmdItnKeyConvError !ItnKeyConversionError
  | ShelleyKeyCmdWrongKeyTypeError
  | ShelleyKeyCmdCardanoAddressSigningKeyFileError
      !(FileError CardanoAddressSigningKeyConversionError)
  | ShelleyKeyCmdNonLegacyKey !FilePath
  | ShelleyKeyCmdExpectedExtendedVerificationKey SomeAddressVerificationKey
  | ShelleyKeyCmdVerificationKeyReadError VerificationKeyTextOrFileError
  deriving Show

renderShelleyKeyCmdError :: ShelleyKeyCmdError -> Text
renderShelleyKeyCmdError err =
  case err of
    ShelleyKeyCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyKeyCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyKeyCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyKeyCmdByronKeyFailure e -> Byron.renderByronKeyFailure e
    ShelleyKeyCmdByronKeyParseError errTxt -> errTxt
    ShelleyKeyCmdItnKeyConvError convErr -> renderConversionError convErr
    ShelleyKeyCmdWrongKeyTypeError ->
      Text.pack "Please use a signing key file when converting ITN BIP32 or Extended keys"
    ShelleyKeyCmdCardanoAddressSigningKeyFileError fileErr ->
      Text.pack (displayError fileErr)
    ShelleyKeyCmdNonLegacyKey fp ->
      "Signing key at: " <> Text.pack fp <> " is not a legacy Byron signing key and should not need to be converted."
    ShelleyKeyCmdVerificationKeyReadError e -> renderVerificationKeyTextOrFileError e
    ShelleyKeyCmdExpectedExtendedVerificationKey someVerKey ->
      "Expected an extended verification key but got: " <> renderSomeAddressVerificationKey someVerKey
