{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Types.Errors.AddressCmdError
  ( AddressCmdError(..)
  , renderAddressCmdError
  ) where

import           Cardano.Api

import           Cardano.CLI.Read
import           Cardano.CLI.Types.Errors.AddressInfoError
import           Cardano.CLI.Types.Key (VerificationKeyTextOrFileError (..),
                   renderVerificationKeyTextOrFileError)

import           Prettyprinter

data AddressCmdError
  = AddressCmdAddressInfoError !AddressInfoError
  | AddressCmdReadKeyFileError !(FileError InputDecodeError)
  | AddressCmdReadScriptFileError !(FileError ScriptDecodeError)
  | AddressCmdVerificationKeyTextOrFileError !VerificationKeyTextOrFileError
  | AddressCmdWriteFileError !(FileError ())
  | AddressCmdExpectedPaymentVerificationKey SomeAddressVerificationKey
  deriving Show

renderAddressCmdError :: AddressCmdError -> Doc ann
renderAddressCmdError = \case
  AddressCmdAddressInfoError addrInfoErr ->
    prettyError addrInfoErr
  AddressCmdReadKeyFileError fileErr ->
    pretty fileErr
  AddressCmdVerificationKeyTextOrFileError vkTextOrFileErr ->
    renderVerificationKeyTextOrFileError vkTextOrFileErr
  AddressCmdReadScriptFileError fileErr ->
    pretty fileErr
  AddressCmdWriteFileError fileErr ->
    pretty fileErr
  AddressCmdExpectedPaymentVerificationKey someAddress ->
    "Expected payment verification key but got: " <> pretty (renderSomeAddressVerificationKey someAddress)
