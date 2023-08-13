{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.ScriptLanguageValidationError
  ( ScriptLanguageValidationError(..)
  ) where

import           Cardano.Api

import           Prelude

import qualified Data.Text as Text

data ScriptLanguageValidationError
  = ScriptLanguageValidationError AnyScriptLanguage AnyCardanoEra
  deriving Show

instance Error ScriptLanguageValidationError where
  displayError = \case
    ScriptLanguageValidationError lang era ->
      "The script language " <> show lang <> " is not supported in the " <>
      Text.unpack (renderEra era) <> " era."
