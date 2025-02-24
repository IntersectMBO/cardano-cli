{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Type.Error.PlutusScriptDecodeError
  ( PlutusScriptDecodeError (..)
  )
where

import Cardano.Api

import Data.Text (Text)

data PlutusScriptDecodeError
  = PlutusScriptDecodeErrorUnknownVersion !Text
  | PlutusScriptJsonDecodeError !JsonDecodeError
  | PlutusScriptDecodeTextEnvelopeError !TextEnvelopeError
  | PlutusScriptDecodeErrorVersionMismatch
      !Text
      -- ^ Script version
      !AnyPlutusScriptVersion
      -- ^ Attempted to decode with version
  deriving Show

instance Error PlutusScriptDecodeError where
  prettyError = \case
    PlutusScriptDecodeErrorUnknownVersion version ->
      "Unknown Plutus script version: " <> pretty version
    PlutusScriptJsonDecodeError err ->
      prettyError err
    PlutusScriptDecodeTextEnvelopeError err ->
      prettyError err
    PlutusScriptDecodeErrorVersionMismatch version (AnyPlutusScriptVersion v) ->
      "Version mismatch in code: script version that was read"
        <+> pretty version
        <+> " but tried to decode script version: "
        <+> pshow v
