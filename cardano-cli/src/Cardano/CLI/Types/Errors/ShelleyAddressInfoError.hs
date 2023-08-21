module Cardano.CLI.Types.Errors.ShelleyAddressInfoError
  ( ShelleyAddressInfoError(..)
  ) where

import           Cardano.Api

import           Data.Text (Text)

newtype ShelleyAddressInfoError = ShelleyAddressInvalid Text
  deriving Show

instance Error ShelleyAddressInfoError where
  displayError (ShelleyAddressInvalid addrTxt) =
    "Invalid address: " <> show addrTxt
