{-# LANGUAGE LambdaCase #-}
module Cardano.CLI.Types.Errors.GovernanceHashError
  ( GovernanceHashError(..)
  ) where

import           Cardano.Api

import           Cardano.Prelude (Exception (displayException), IOException)

data GovernanceHashError
  = GovernanceHashReadFileError FilePath IOException
  deriving Show

instance Error GovernanceHashError where
  displayError = \case
    GovernanceHashReadFileError filepath exc ->
      "Cannot read " <> filepath <> ": " <> displayException exc
