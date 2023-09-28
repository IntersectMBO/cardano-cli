{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Types.Errors.KeyCmdError
  ( KeyCmdError (..)
  , renderKeyCmdError
  ) where

import Cardano.Api

import qualified Cardano.CLI.Byron.Key as Byron
import Cardano.CLI.Types.Errors.CardanoAddressSigningKeyConversionError
import Cardano.CLI.Types.Errors.ItnKeyConversionError
import Cardano.CLI.Types.Key

import Data.Text (Text)
import qualified Data.Text as Text

data KeyCmdError
  = KeyCmdReadFileError !(FileError TextEnvelopeError)
  | KeyCmdReadKeyFileError !(FileError InputDecodeError)
  | KeyCmdWriteFileError !(FileError ())
  | KeyCmdByronKeyFailure !Byron.ByronKeyFailure
  | -- | Text representation of the parse error. Unfortunately, the actual
    -- error type isn't exported.
    KeyCmdByronKeyParseError
      !Text
  | KeyCmdItnKeyConvError !ItnKeyConversionError
  | KeyCmdWrongKeyTypeError
  | KeyCmdCardanoAddressSigningKeyFileError
      !(FileError CardanoAddressSigningKeyConversionError)
  | KeyCmdNonLegacyKey !FilePath
  | KeyCmdExpectedExtendedVerificationKey SomeAddressVerificationKey
  | KeyCmdVerificationKeyReadError VerificationKeyTextOrFileError
  deriving (Show)

renderKeyCmdError :: KeyCmdError -> Text
renderKeyCmdError err =
  case err of
    KeyCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    KeyCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)
    KeyCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    KeyCmdByronKeyFailure e -> Byron.renderByronKeyFailure e
    KeyCmdByronKeyParseError errTxt -> errTxt
    KeyCmdItnKeyConvError convErr -> renderConversionError convErr
    KeyCmdWrongKeyTypeError ->
      Text.pack "Please use a signing key file when converting ITN BIP32 or Extended keys"
    KeyCmdCardanoAddressSigningKeyFileError fileErr ->
      Text.pack (displayError fileErr)
    KeyCmdNonLegacyKey fp ->
      "Signing key at: "
        <> Text.pack fp
        <> " is not a legacy Byron signing key and should not need to be converted."
    KeyCmdVerificationKeyReadError e -> renderVerificationKeyTextOrFileError e
    KeyCmdExpectedExtendedVerificationKey someVerKey ->
      "Expected an extended verification key but got: " <> renderSomeAddressVerificationKey someVerKey
