{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Type.Error.TextViewFileError
  ( TextViewFileError (..)
  , renderTextViewFileError
  )
where

import Cardano.Api

import Cardano.CLI.Helper (HelpersError, renderHelpersError)

data TextViewFileError
  = TextViewReadFileError (FileError TextEnvelopeError)
  | TextViewWriteFileError (FileError TextEnvelopeError)
  | TextViewCBORPrettyPrintError !HelpersError
  deriving Show

renderTextViewFileError :: TextViewFileError -> Doc ann
renderTextViewFileError = \case
  TextViewReadFileError fileErr ->
    prettyError fileErr
  TextViewWriteFileError fileErr ->
    prettyError fileErr
  TextViewCBORPrettyPrintError hlprsErr ->
    "Error pretty printing CBOR: " <> renderHelpersError hlprsErr
