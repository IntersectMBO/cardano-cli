
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
import qualified Data.Text as Text

data KeyCmdError
  = KeyCmdReadFileError
      !(FileError TextEnvelopeError)
  | KeyCmdReadKeyFileError
      !(FileError InputDecodeError)
  | KeyCmdWriteFileError
      !(FileError ())
  | KeyCmdByronKeyFailureError
      !Byron.ByronKeyFailure
  | KeyCmdByronKeyParseError
      !Text
      -- ^ Text representation of the parse error. Unfortunately, the actual
      -- error type isn't exported.
  | KeyCmdItnKeyConvError
      !ItnKeyConversionError
  | KeyCmdWrongKeyTypeError
  | KeyCmdCardanoAddressSigningKeyFileError
      !(FileError CardanoAddressSigningKeyConversionError)
  | KeyCmdNonLegacyKeyError
      !FilePath
  | KeyCmdExpectedExtendedVerificationKeyError
      SomeAddressVerificationKey
  | KeyCmdVerificationKeyReadError
      VerificationKeyTextOrFileError
  deriving Show

renderKeyCmdError :: KeyCmdError -> Text
renderKeyCmdError err =
  case err of
    KeyCmdReadFileError fileErr ->
      Text.pack (displayError fileErr)
    KeyCmdReadKeyFileError fileErr ->
      Text.pack (displayError fileErr)
    KeyCmdWriteFileError fileErr ->
      Text.pack (displayError fileErr)
    KeyCmdByronKeyFailureError e ->
      Byron.renderByronKeyFailure e
    KeyCmdByronKeyParseError errTxt ->
      errTxt
    KeyCmdItnKeyConvError convErr ->
      renderItnKeyConversionError convErr
    KeyCmdWrongKeyTypeError ->
      Text.pack "Please use a signing key file when converting ITN BIP32 or Extended keys"
    KeyCmdCardanoAddressSigningKeyFileError fileErr ->
      Text.pack (displayError fileErr)
    KeyCmdNonLegacyKeyError fp ->
      "Signing key at: " <> Text.pack fp <> " is not a legacy Byron signing key and should not need to be converted."
    KeyCmdVerificationKeyReadError e ->
      renderVerificationKeyTextOrFileError e
    KeyCmdExpectedExtendedVerificationKeyError someVerKey ->
      "Expected an extended verification key but got: " <> renderSomeAddressVerificationKey someVerKey
