{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.GovernanceCommitteeError
  ( GovernanceCommitteeError(..)
  ) where

import           Cardano.Api

data GovernanceCommitteeError
  = GovernanceCommitteeCmdKeyDecodeError InputDecodeError
  | GovernanceCommitteeCmdKeyReadError (FileError InputDecodeError)
  | GovernanceCommitteeCmdTextEnvReadFileError (FileError TextEnvelopeError)
  | GovernanceCommitteeCmdTextEnvWriteError (FileError ())
  | GovernanceCommitteeCmdWriteFileError (FileError ())
  deriving Show

instance Error GovernanceCommitteeError where
  displayError = \case
    GovernanceCommitteeCmdKeyDecodeError e ->
      "Cannot decode key: " <> displayError e
    GovernanceCommitteeCmdKeyReadError e ->
      "Cannot read key: " <> displayError e
    GovernanceCommitteeCmdWriteFileError e ->
      "Cannot write file: " <> displayError e
    GovernanceCommitteeCmdTextEnvReadFileError e ->
      "Cannot read text envelope file: " <> displayError e
    GovernanceCommitteeCmdTextEnvWriteError e ->
      "Cannot write text envelope file: " <> displayError e
