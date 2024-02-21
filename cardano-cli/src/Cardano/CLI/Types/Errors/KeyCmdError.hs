{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Types.Errors.KeyCmdError
  ( KeyCmdError(..)
  , renderKeyCmdError
  ) where

import           Cardano.Api

import qualified Cardano.CLI.Byron.Key as Byron
import           Cardano.CLI.Types.Errors.CardanoAddressSigningKeyConversionError
import           Cardano.CLI.Types.Errors.ItnKeyConversionError
import           Cardano.CLI.Types.Key

import           Data.Text (Text)

data KeyCmdError
  = KeyCmdReadFileError !(FileError TextEnvelopeError)
  | KeyCmdReadKeyFileError !(FileError InputDecodeError)
  | KeyCmdWriteFileError !(FileError ())
  | KeyCmdByronKeyFailure !Byron.ByronKeyFailure
  | KeyCmdByronKeyParseError
      !Text
      -- ^ Text representation of the parse error. Unfortunately, the actual
      -- error type isn't exported.
  | KeyCmdItnKeyConvError !ItnKeyConversionError
  | KeyCmdWrongKeyTypeError
  | KeyCmdCardanoAddressSigningKeyFileError
      !(FileError CardanoAddressSigningKeyConversionError)
  | KeyCmdNonLegacyKey !FilePath
  | KeyCmdExpectedExtendedVerificationKey SomeAddressVerificationKey
  | KeyCmdVerificationKeyReadError VerificationKeyTextOrFileError
  deriving Show

renderKeyCmdError :: KeyCmdError -> Doc ann
renderKeyCmdError err =
  case err of
    KeyCmdReadFileError fileErr ->
      prettyError fileErr
    KeyCmdReadKeyFileError fileErr ->
      prettyError fileErr
    KeyCmdWriteFileError fileErr ->
      prettyError fileErr
    KeyCmdByronKeyFailure e ->
      Byron.renderByronKeyFailure e
    KeyCmdByronKeyParseError errTxt ->
      pretty errTxt
    KeyCmdItnKeyConvError convErr ->
      renderConversionError convErr
    KeyCmdWrongKeyTypeError ->
      "Please use a signing key file when converting ITN BIP32 or Extended keys"
    KeyCmdCardanoAddressSigningKeyFileError fileErr ->
      prettyError fileErr
    KeyCmdNonLegacyKey fp ->
      "Signing key at: " <> pretty fp <> " is not a legacy Byron signing key and should not need to be converted."
    KeyCmdVerificationKeyReadError e ->
      renderVerificationKeyTextOrFileError e
    KeyCmdExpectedExtendedVerificationKey someVerKey ->
      "Expected an extended verification key but got: " <> pretty (renderSomeAddressVerificationKey someVerKey)
