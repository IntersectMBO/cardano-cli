{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.ScriptDecodeError
  ( ScriptDecodeError (..)
  )
where

import           Cardano.Api

import           Data.Text

--
-- Handling decoding the variety of script languages and formats
--

data ScriptDecodeError
  = ScriptDecodeTextEnvelopeError TextEnvelopeError
  | ScriptDecodeSimpleScriptError JsonDecodeError
  | ScriptDecodeUnknownPlutusScriptVersion Text
  deriving Show

instance Error ScriptDecodeError where
  prettyError = \case
    ScriptDecodeTextEnvelopeError err ->
      "Error decoding script: " <> prettyError err
    ScriptDecodeSimpleScriptError err ->
      "Syntax error in script: " <> prettyError err
    ScriptDecodeUnknownPlutusScriptVersion version ->
      "Unknown Plutus script version: " <> pshow version
