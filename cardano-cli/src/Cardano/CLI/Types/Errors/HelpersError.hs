{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.HelpersError
  ( HelpersError(..)
  , renderHelpersError
  ) where

import           Codec.CBOR.Read (DeserialiseFailure)
import           Control.Exception (IOException)
import           Data.Text (Text)
import qualified Data.Text as Text

data HelpersError
  = CBORPrettyPrintError !DeserialiseFailure
  | CBORDecodingError !DeserialiseFailure
  | IOError' !FilePath !IOException
  | OutputMustNotAlreadyExist FilePath
  | ReadCBORFileFailure !FilePath !Text
  deriving Show

renderHelpersError :: HelpersError -> Text
renderHelpersError = \case
  OutputMustNotAlreadyExist fp ->
    "Output file/directory must not already exist: " <> Text.pack fp
  ReadCBORFileFailure fp err' ->
    "CBOR read failure at: " <> Text.pack fp <> Text.pack (show err')
  CBORPrettyPrintError err' ->
    "Error with CBOR decoding: " <> Text.pack (show err')
  CBORDecodingError err' ->
    "Error with CBOR decoding: " <> Text.pack (show err')
  IOError' fp ioE ->
    "Error at: " <> Text.pack fp <> " Error: " <> Text.pack (show ioE)
