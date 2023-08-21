module Cardano.CLI.Types.Errors.CardanoAddressSigningKeyConversionError
  ( CardanoAddressSigningKeyConversionError(..)
  , renderCardanoAddressSigningKeyConversionError
  ) where

import           Cardano.Api

import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text as Text

-- | An error that can occur while converting a @cardano-address@ extended
-- signing key.
data CardanoAddressSigningKeyConversionError
  = CardanoAddressSigningKeyBech32DecodeError !Bech32DecodeError
  -- ^ There was an error in decoding the string as Bech32.
  | CardanoAddressSigningKeyDeserialisationError !ByteString
  -- ^ There was an error in converting the @cardano-address@ extended signing
  -- key.
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
