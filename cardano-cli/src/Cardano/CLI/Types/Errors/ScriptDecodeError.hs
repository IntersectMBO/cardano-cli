{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.ScriptDecodeError
  ( ScriptDecodeError (..)
  ) where

import Cardano.Api

--
-- Handling decoding the variety of script languages and formats
--

data ScriptDecodeError
  = ScriptDecodeTextEnvelopeError TextEnvelopeError
  | ScriptDecodeSimpleScriptError JsonDecodeError
  deriving (Show)

instance Error ScriptDecodeError where
  displayError = \case
    ScriptDecodeTextEnvelopeError err ->
      "Error decoding script: " ++ displayError err
    ScriptDecodeSimpleScriptError err ->
      "Syntax error in script: " ++ displayError err
