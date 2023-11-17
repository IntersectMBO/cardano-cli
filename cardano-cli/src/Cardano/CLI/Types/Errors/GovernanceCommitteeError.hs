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
  prettyError = \case
    GovernanceCommitteeCmdKeyDecodeError e ->
      "Cannot decode key: " <> prettyError e
    GovernanceCommitteeCmdKeyReadError e ->
      "Cannot read key: " <> prettyError e
    GovernanceCommitteeCmdWriteFileError e ->
      "Cannot write file: " <> prettyError e
    GovernanceCommitteeCmdTextEnvReadFileError e ->
      "Cannot read text envelope file: " <> prettyError e
    GovernanceCommitteeCmdTextEnvWriteError e ->
      "Cannot write text envelope file: " <> prettyError e
