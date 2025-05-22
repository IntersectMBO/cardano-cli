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

import Cardano.CLI.Type.Key

data KeyCmdError
  = KeyCmdReadMnemonicFileError !(FileError ())
  | KeyCmdWriteFileError !(FileError ())
  | KeyCmdMnemonicError MnemonicToSigningKeyError
  | KeyCmdWrongNumOfMnemonics
      !Int
      -- ^ Expected number of mnemonics
      !Int
      -- ^ Actual number of mnemonics
  | KeyCmdWrongKeyTypeError
  | KeyCmdExpectedExtendedVerificationKey SomeAddressVerificationKey
  | KeyCmdVerificationKeyReadError VerificationKeyTextOrFileError
  deriving Show

instance Error KeyCmdError where
  prettyError = renderKeyCmdError

renderKeyCmdError :: KeyCmdError -> Doc ann
renderKeyCmdError err =
  case err of
    KeyCmdReadMnemonicFileError fileErr ->
      "Error reading mnemonic file: " <> prettyError fileErr
    KeyCmdWriteFileError fileErr ->
      prettyError fileErr
    KeyCmdMnemonicError mnemonicErr ->
      "Error converting the mnemonic into a key: " <> prettyError mnemonicErr
    KeyCmdWrongNumOfMnemonics expected actual ->
      "Internal error: expected " <> pretty expected <> " mnemonics, but got " <> pretty actual
    KeyCmdWrongKeyTypeError ->
      "Please use a signing key file when converting ITN BIP32 or Extended keys"
    KeyCmdVerificationKeyReadError e ->
      renderVerificationKeyTextOrFileError e
    KeyCmdExpectedExtendedVerificationKey someVerKey ->
      "Expected an extended verification key but got: "
        <> pretty (renderSomeAddressVerificationKey someVerKey)
