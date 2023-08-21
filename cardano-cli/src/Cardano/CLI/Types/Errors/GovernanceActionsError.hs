{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.GovernanceActionsError
  ( GovernanceActionsError(..)
  ) where

import           Cardano.Api

import           Data.Text.Encoding.Error

data GovernanceActionsError
  = GovernanceActionsCmdNonUtf8EncodedConstitution UnicodeException
  | GovernanceActionsCmdReadFileError (FileError InputDecodeError)
  | GovernanceActionsCmdReadTextEnvelopeFileError (FileError TextEnvelopeError)
  | GovernanceActionsCmdWriteFileError (FileError ())
  deriving Show

instance Error GovernanceActionsError where
  displayError = \case
    GovernanceActionsCmdNonUtf8EncodedConstitution e ->
      "Cannot read constitution: " <> show e
    GovernanceActionsCmdReadFileError e ->
      "Cannot read file: " <> displayError e
    GovernanceActionsCmdReadTextEnvelopeFileError e ->
      "Cannot read text envelope file: " <> displayError e
    GovernanceActionsCmdWriteFileError e ->
      "Cannot write file: " <> displayError e
