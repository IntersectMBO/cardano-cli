{-# LANGUAGE DataKinds #-}

module Cardano.CLI.Types.Errors.TextViewFileError
  ( TextViewFileError (..)
  , renderTextViewFileError
  ) where

import Cardano.Api

import Cardano.CLI.Helpers (HelpersError, renderHelpersError)

import Data.Text (Text)
import qualified Data.Text as Text

data TextViewFileError
  = TextViewReadFileError (FileError TextEnvelopeError)
  | TextViewCBORPrettyPrintError !HelpersError
  deriving (Show)

renderTextViewFileError :: TextViewFileError -> Text
renderTextViewFileError err =
  case err of
    TextViewReadFileError fileErr -> Text.pack (displayError fileErr)
    TextViewCBORPrettyPrintError hlprsErr ->
      "Error pretty printing CBOR: " <> renderHelpersError hlprsErr
