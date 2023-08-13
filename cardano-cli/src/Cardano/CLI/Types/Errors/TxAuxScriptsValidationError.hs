{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.TxAuxScriptsValidationError
  ( TxAuxScriptsValidationError(..)
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Errors.ScriptLanguageValidationError

import qualified Data.Text as Text

data TxAuxScriptsValidationError
  = TxAuxScriptsNotSupportedInEra AnyCardanoEra
  | TxAuxScriptsLanguageError ScriptLanguageValidationError
  deriving Show

instance Error TxAuxScriptsValidationError where
  displayError = \case
    TxAuxScriptsNotSupportedInEra era ->
      "Transaction auxiliary scripts are not supported in " <> Text.unpack (renderEra era)
    TxAuxScriptsLanguageError e ->
      "Transaction auxiliary scripts error: " <> displayError e
