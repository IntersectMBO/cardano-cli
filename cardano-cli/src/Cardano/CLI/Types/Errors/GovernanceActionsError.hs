{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.GovernanceActionsError
  ( GovernanceActionsError(..)
  ) where

import           Cardano.Api

import           Cardano.CLI.Read

data GovernanceActionsError
  = GovernanceActionsCmdConstitutionError ConstitutionError
  | GovernanceActionsCmdReadFileError (FileError InputDecodeError)
  | GovernanceActionsCmdReadTextEnvelopeFileError (FileError TextEnvelopeError)
  | GovernanceActionsCmdWriteFileError (FileError ())
  deriving Show

instance Error GovernanceActionsError where
  displayError = \case
    GovernanceActionsCmdConstitutionError e ->
      "Cannot read constitution: " <> show e -- TODO Conway render this properly
    GovernanceActionsCmdReadFileError e ->
      "Cannot read file: " <> displayError e
    GovernanceActionsCmdReadTextEnvelopeFileError e ->
      "Cannot read text envelope file: " <> displayError e
    GovernanceActionsCmdWriteFileError e ->
      "Cannot write file: " <> displayError e
