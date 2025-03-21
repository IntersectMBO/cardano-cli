{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Type.Error.KeyCmdError
  ( KeyCmdError (..)
  , renderKeyCmdError
  )
where

import Cardano.Api

import Cardano.CLI.Byron.Key qualified as Byron
import Cardano.CLI.Type.Error.CardanoAddressSigningKeyConversionError
import Cardano.CLI.Type.Error.ItnKeyConversionError
import Cardano.CLI.Type.Key

import Data.Text (Text)

data KeyCmdError
  = KeyCmdReadFileError !(FileError TextEnvelopeError)
  | KeyCmdReadKeyFileError !(FileError InputDecodeError)
  | KeyCmdReadMnemonicFileError !(FileError ())
  | KeyCmdWriteFileError !(FileError ())
  | KeyCmdMnemonicError MnemonicToSigningKeyError
  | KeyCmdWrongNumOfMnemonics
      !Int
      -- ^ Expected number of mnemonics
      !Int
      -- ^ Actual number of mnemonics
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
  deriving Show

renderKeyCmdError :: KeyCmdError -> Doc ann
renderKeyCmdError err =
  case err of
    KeyCmdReadFileError fileErr ->
      prettyError fileErr
    KeyCmdReadKeyFileError fileErr ->
      prettyError fileErr
    KeyCmdReadMnemonicFileError fileErr ->
      "Error reading mnemonic file: " <> prettyError fileErr
    KeyCmdWriteFileError fileErr ->
      prettyError fileErr
    KeyCmdMnemonicError mnemonicErr ->
      "Error converting the mnemonic into a key: " <> prettyError mnemonicErr
    KeyCmdWrongNumOfMnemonics expected actual ->
      "Internal error: expected " <> pretty expected <> " mnemonics, but got " <> pretty actual
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
      "Signing key at: "
        <> pretty fp
        <> " is not a legacy Byron signing key and should not need to be converted."
    KeyCmdVerificationKeyReadError e ->
      renderVerificationKeyTextOrFileError e
    KeyCmdExpectedExtendedVerificationKey someVerKey ->
      "Expected an extended verification key but got: "
        <> pretty (renderSomeAddressVerificationKey someVerKey)
