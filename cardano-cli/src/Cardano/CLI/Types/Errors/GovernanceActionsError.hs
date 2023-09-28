{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.GovernanceActionsError
  ( GovernanceActionsError (..)
  ) where

import Cardano.Api

import Cardano.CLI.Read

data GovernanceActionsError
  = GovernanceActionsCmdConstitutionError ConstitutionError
  | GovernanceActionsCmdProposalError ProposalError
  | GovernanceActionsCmdReadFileError (FileError InputDecodeError)
  | GovernanceActionsCmdReadTextEnvelopeFileError (FileError TextEnvelopeError)
  | GovernanceActionsCmdWriteFileError (FileError ())
  deriving (Show)

instance Error GovernanceActionsError where
  displayError = \case
    GovernanceActionsCmdProposalError e ->
      "Cannot read proposal: " <> show e -- TODO Conway render this properly
    GovernanceActionsCmdConstitutionError e ->
      "Cannot read constitution: " <> show e -- TODO Conway render this properly
    GovernanceActionsCmdReadFileError e ->
      "Cannot read file: " <> displayError e
    GovernanceActionsCmdReadTextEnvelopeFileError e ->
      "Cannot read text envelope file: " <> displayError e
    GovernanceActionsCmdWriteFileError e ->
      "Cannot write file: " <> displayError e
