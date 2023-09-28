{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Types.Errors.AddressCmdError
  ( AddressCmdError (..)
  , renderAddressCmdError
  ) where

import Cardano.Api

import Cardano.CLI.Read
import Cardano.CLI.Types.Errors.AddressInfoError
import Cardano.CLI.Types.Key
  ( VerificationKeyTextOrFileError (..)
  , renderVerificationKeyTextOrFileError
  )

import Data.Text (Text)
import Data.Text qualified as Text

data AddressCmdError
  = AddressCmdAddressInfoError !AddressInfoError
  | AddressCmdReadKeyFileError !(FileError InputDecodeError)
  | AddressCmdReadScriptFileError !(FileError ScriptDecodeError)
  | AddressCmdVerificationKeyTextOrFileError !VerificationKeyTextOrFileError
  | AddressCmdWriteFileError !(FileError ())
  | AddressCmdExpectedPaymentVerificationKey SomeAddressVerificationKey
  deriving (Show)

renderAddressCmdError :: AddressCmdError -> Text
renderAddressCmdError err =
  case err of
    AddressCmdAddressInfoError addrInfoErr ->
      Text.pack (displayError addrInfoErr)
    AddressCmdReadKeyFileError fileErr ->
      Text.pack (displayError fileErr)
    AddressCmdVerificationKeyTextOrFileError vkTextOrFileErr ->
      renderVerificationKeyTextOrFileError vkTextOrFileErr
    AddressCmdReadScriptFileError fileErr ->
      Text.pack (displayError fileErr)
    AddressCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    AddressCmdExpectedPaymentVerificationKey someAddress ->
      "Expected payment verification key but got: " <> renderSomeAddressVerificationKey someAddress
