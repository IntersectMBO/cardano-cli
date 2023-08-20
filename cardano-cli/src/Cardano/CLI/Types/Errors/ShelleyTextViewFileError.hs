{-# LANGUAGE DataKinds #-}

module Cardano.CLI.Types.Errors.ShelleyTextViewFileError
  ( ShelleyTextViewFileError(..)
  , renderShelleyTextViewFileError
  ) where

import           Cardano.Api

import           Cardano.CLI.Helpers (HelpersError, renderHelpersError)

import           Data.Text (Text)
import qualified Data.Text as Text

data ShelleyTextViewFileError
  = TextViewReadFileError (FileError TextEnvelopeError)
  | TextViewCBORPrettyPrintError !HelpersError
  deriving Show

renderShelleyTextViewFileError :: ShelleyTextViewFileError -> Text
renderShelleyTextViewFileError err =
  case err of
    TextViewReadFileError fileErr -> Text.pack (displayError fileErr)
    TextViewCBORPrettyPrintError hlprsErr ->
      "Error pretty printing CBOR: " <> renderHelpersError hlprsErr
