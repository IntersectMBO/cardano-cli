{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Types.Errors.ItnKeyConversionError
  ( ItnKeyConversionError (..)
  , renderConversionError
  )
where

import Cardano.Api

import Control.Exception (Exception (..), IOException)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC

-- | An error that can occur while converting an Incentivized Testnet (ITN)
-- key.
data ItnKeyConversionError
  = ItnKeyBech32DecodeError !Bech32DecodeError
  | ItnReadBech32FileError !FilePath !IOException
  | ItnSigningKeyDeserialisationError !ByteString
  | ItnVerificationKeyDeserialisationError !ByteString
  deriving Show

-- | Render an error message for an 'ItnKeyConversionError'.
renderConversionError :: ItnKeyConversionError -> Doc ann
renderConversionError = \case
  ItnKeyBech32DecodeError decErr ->
    "Error decoding Bech32 key: " <> prettyError decErr
  ItnReadBech32FileError fp readErr ->
    "Error reading Bech32 key at: "
      <> pshow fp
      <> " Error: "
      <> pshow (displayException readErr)
  ItnSigningKeyDeserialisationError _sKey ->
    -- Sensitive data, such as the signing key, is purposely not included in
    -- the error message.
    "Error deserialising signing key."
  ItnVerificationKeyDeserialisationError vKey ->
    "Error deserialising verification key: " <> pshow (BSC.unpack vKey)
