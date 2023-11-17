{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.GovernanceCommitteeError
  ( GovernanceCommitteeError(..)
  ) where

import           Cardano.Api
import           Cardano.Api.Pretty

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
      "Cannot read key: " <> pretty e
    GovernanceCommitteeCmdWriteFileError e ->
      "Cannot write file: " <> pretty e
    GovernanceCommitteeCmdTextEnvReadFileError e ->
      "Cannot read text envelope file: " <> pretty e
    GovernanceCommitteeCmdTextEnvWriteError e ->
      "Cannot write text envelope file: " <> pretty e
