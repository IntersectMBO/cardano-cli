{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Type.Error.ScriptDataError
  ( ScriptDataError (..)
  , renderScriptDataError
  )
where

import Cardano.Api

import Cardano.Binary qualified as CBOR

data ScriptDataError
  = ScriptDataErrorFile (FileError ())
  | ScriptDataErrorJsonParse !FilePath !String
  | ScriptDataErrorConversion !FilePath !ScriptDataJsonError
  | ScriptDataErrorValidation !FilePath !ScriptDataRangeError
  | ScriptDataErrorMetadataDecode !FilePath !CBOR.DecoderError
  | ScriptDataErrorJsonBytes !ScriptDataJsonBytesError
  deriving Show

instance Error ScriptDataError where
  prettyError = renderScriptDataError

renderScriptDataError :: ScriptDataError -> Doc ann
renderScriptDataError = \case
  ScriptDataErrorFile err ->
    prettyError err
  ScriptDataErrorJsonParse fp jsonErr ->
    "Invalid JSON format in file:" <+> pshow fp <> "\nJSON parse error:" <+> pretty jsonErr
  ScriptDataErrorConversion fp sDataJsonErr ->
    "Error reading metadata at:" <+> pshow fp <> "\n" <> prettyError sDataJsonErr
  ScriptDataErrorValidation fp sDataRangeErr ->
    "Error validating script data at:" <+> pshow fp <> ":\n" <> prettyError sDataRangeErr
  ScriptDataErrorMetadataDecode fp decoderErr ->
    "Error decoding CBOR metadata at:" <+> pshow fp <+> "Error:" <+> pshow decoderErr
  ScriptDataErrorJsonBytes e ->
    prettyError e
