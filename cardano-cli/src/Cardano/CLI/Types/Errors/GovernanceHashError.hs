{-# LANGUAGE LambdaCase #-}
module Cardano.CLI.Types.Errors.GovernanceHashError
  ( GovernanceHashError(..)
  ) where

import           Cardano.Api
import           Cardano.Api.Pretty

import           Cardano.Prelude (Exception (displayException), IOException)

data GovernanceHashError
  = GovernanceHashReadFileError !FilePath !IOException
  | GovernanceHashWriteFileError !(FileError ())
  deriving Show

instance Error GovernanceHashError where
  prettyError = \case
    GovernanceHashReadFileError filepath exc ->
      "Cannot read " <> pretty filepath <> ": " <> pretty (displayException exc)
      "Cannot read " <> filepath <> ": " <> displayException exc
    GovernanceHashWriteFileError fileErr ->
      displayError fileErr
