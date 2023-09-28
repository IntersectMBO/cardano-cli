module Cardano.CLI.Types.Errors.CardanoAddressSigningKeyConversionError
  ( CardanoAddressSigningKeyConversionError (..)
  , renderCardanoAddressSigningKeyConversionError
  ) where

import Cardano.Api

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text

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
  displayError = Text.unpack . renderCardanoAddressSigningKeyConversionError

-- | Render an error message for a 'CardanoAddressSigningKeyConversionError'.
renderCardanoAddressSigningKeyConversionError
  :: CardanoAddressSigningKeyConversionError
  -> Text
renderCardanoAddressSigningKeyConversionError err =
  case err of
    CardanoAddressSigningKeyBech32DecodeError decErr ->
      Text.pack (displayError decErr)
    CardanoAddressSigningKeyDeserialisationError _bs ->
      -- Sensitive data, such as the signing key, is purposely not included in
      -- the error message.
      "Error deserialising cardano-address signing key."
