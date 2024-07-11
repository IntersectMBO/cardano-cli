{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.CardanoAddressSigningKeyConversionError
  ( CardanoAddressSigningKeyConversionError (..)
  )
where

import           Cardano.Api

import           Data.ByteString (ByteString)

-- | An error that can occur while converting a @cardano-address@ extended
-- signing key.
data CardanoAddressSigningKeyConversionError
  = -- | There was an error in decoding the string as Bech32.
    CardanoAddressSigningKeyBech32DecodeError !Bech32DecodeError
  | -- | There was an error in converting the @cardano-address@ extended signing
    -- key.
    CardanoAddressSigningKeyDeserialisationError !ByteString
  deriving (Show, Eq)

instance Error CardanoAddressSigningKeyConversionError where
  prettyError = \case
    CardanoAddressSigningKeyBech32DecodeError decErr ->
      prettyError decErr
    CardanoAddressSigningKeyDeserialisationError _bs ->
      -- Sensitive data, such as the signing key, is purposely not included in
      -- the error message.
      "Error deserialising cardano-address signing key."
