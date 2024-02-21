{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.AddressInfoError
  ( AddressInfoError(..)
  ) where

import           Cardano.Api

import           Data.Text (Text)

newtype AddressInfoError = ShelleyAddressInvalid Text
  deriving Show

instance Error AddressInfoError where
  prettyError = \case
    ShelleyAddressInvalid addrTxt ->
      "Invalid address: " <> pshow addrTxt
