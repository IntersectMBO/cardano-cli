module Cardano.CLI.Types.Errors.AddressInfoError
  ( AddressInfoError(..)
  ) where

import           Cardano.Api

import           Data.Text (Text)

newtype AddressInfoError = ShelleyAddressInvalid Text
  deriving Show

instance Error AddressInfoError where
  displayError (ShelleyAddressInvalid addrTxt) =
    "Invalid address: " <> show addrTxt
