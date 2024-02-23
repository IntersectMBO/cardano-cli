{-# LANGUAGE LambdaCase #-}
module Cardano.CLI.Types.Errors.GovernanceHashError
  ( GovernanceHashError(..)
  ) where

import           Cardano.Api

import           Cardano.CLI.Read (ScriptDecodeError)
import           Cardano.Prelude (Exception (displayException), IOException)

data GovernanceHashError
  = GovernanceHashReadFileError !FilePath !IOException
  | GovernanceHashWriteFileError !(FileError ())
  | GovernanceHashReadScriptError !FilePath !(FileError ScriptDecodeError)
  deriving Show

instance Error GovernanceHashError where
  prettyError = \case
    GovernanceHashReadFileError filepath exc ->
      "Cannot read " <> pretty filepath <> ": " <> pretty (displayException exc)
    GovernanceHashWriteFileError fileErr ->
      prettyError fileErr
    GovernanceHashReadScriptError filepath err ->
      "Cannot read script at " <> pretty filepath <> ": " <> prettyError err
