{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.HashCmdError
  ( HashCmdError(..)
  ) where

import           Cardano.Api

import           Cardano.CLI.Read (ScriptDecodeError)
import           Cardano.Prelude (Exception (displayException), IOException)

data HashCmdError
  = HashReadFileError !FilePath !IOException
  | HashWriteFileError !(FileError ())
  | HashReadScriptError !FilePath !(FileError ScriptDecodeError)
  deriving Show

instance Error HashCmdError where
  prettyError = \case
    HashReadFileError filepath exc ->
      "Cannot read " <> pretty filepath <> ": " <> pretty (displayException exc)
    HashWriteFileError fileErr ->
      prettyError fileErr
    HashReadScriptError filepath err ->
      "Cannot read script at " <> pretty filepath <> ": " <> prettyError err
