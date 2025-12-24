{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Type.Error.ScriptDecodeError
  ( ScriptDecodeError (..)
  )
where

import Cardano.Api

--
-- Handling decoding the variety of script languages and formats
--

data ScriptDecodeError
  = ScriptDecodeTextEnvelopeError TextEnvelopeError
  | ScriptDecodeSimpleScriptError JsonDecodeError
  deriving Show

instance Error ScriptDecodeError where
  prettyError = \case
    ScriptDecodeTextEnvelopeError err ->
      "Error decoding script:" <+> prettyError err
    ScriptDecodeSimpleScriptError err ->
      "Syntax error in script:" <+> prettyError err
