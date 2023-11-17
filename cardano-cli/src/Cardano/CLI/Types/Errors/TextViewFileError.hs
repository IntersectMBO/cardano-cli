{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.TextViewFileError
  ( TextViewFileError(..)
  , renderTextViewFileError
  ) where

import           Cardano.Api

import           Cardano.CLI.Helpers (HelpersError, renderHelpersError)

import           Prettyprinter

data TextViewFileError
  = TextViewReadFileError (FileError TextEnvelopeError)
  | TextViewCBORPrettyPrintError !HelpersError
  deriving Show

renderTextViewFileError :: TextViewFileError -> Doc ann
renderTextViewFileError = \case
  TextViewReadFileError fileErr ->
    pretty fileErr
  TextViewCBORPrettyPrintError hlprsErr ->
    "Error pretty printing CBOR: " <> renderHelpersError hlprsErr
