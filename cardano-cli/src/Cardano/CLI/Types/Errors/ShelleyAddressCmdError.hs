{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Types.Errors.ShelleyAddressCmdError
  ( ShelleyAddressCmdError(..)
  , renderShelleyAddressCmdError
  ) where

import           Cardano.Api

import           Cardano.CLI.Read
import           Cardano.CLI.Types.Errors.ShelleyAddressInfoError
import           Cardano.CLI.Types.Key (VerificationKeyTextOrFileError (..),
                   renderVerificationKeyTextOrFileError)

import           Data.Text (Text)
import qualified Data.Text as Text

data ShelleyAddressCmdError
  = ShelleyAddressCmdAddressInfoError !ShelleyAddressInfoError
  | ShelleyAddressCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyAddressCmdReadScriptFileError !(FileError ScriptDecodeError)
  | ShelleyAddressCmdVerificationKeyTextOrFileError !VerificationKeyTextOrFileError
  | ShelleyAddressCmdWriteFileError !(FileError ())
  | ShelleyAddressCmdExpectedPaymentVerificationKey SomeAddressVerificationKey
  deriving Show

renderShelleyAddressCmdError :: ShelleyAddressCmdError -> Text
renderShelleyAddressCmdError err =
  case err of
    ShelleyAddressCmdAddressInfoError addrInfoErr ->
      Text.pack (displayError addrInfoErr)
    ShelleyAddressCmdReadKeyFileError fileErr ->
      Text.pack (displayError fileErr)
    ShelleyAddressCmdVerificationKeyTextOrFileError vkTextOrFileErr ->
      renderVerificationKeyTextOrFileError vkTextOrFileErr
    ShelleyAddressCmdReadScriptFileError fileErr ->
      Text.pack (displayError fileErr)
    ShelleyAddressCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyAddressCmdExpectedPaymentVerificationKey someAddress ->
      "Expected payment verification key but got: " <> renderSomeAddressVerificationKey someAddress
