{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Type.Error.GovernanceCommitteeError
  ( GovernanceCommitteeError (..)
  )
where

import Cardano.Api

import Cardano.CLI.Type.Error.HashCmdError (HashCheckError)
import Cardano.CLI.Type.Error.ScriptDecodeError

import Control.Exception (displayException)

data GovernanceCommitteeError
  = GovernanceCommitteeCmdKeyDecodeError InputDecodeError
  | GovernanceCommitteeCmdKeyReadError (FileError InputDecodeError)
  | GovernanceCommitteeCmdScriptReadError (FileError ScriptDecodeError)
  | GovernanceCommitteeCmdTextEnvReadFileError (FileError TextEnvelopeError)
  | GovernanceCommitteeCmdTextEnvWriteError (FileError ())
  | GovernanceCommitteeCmdWriteFileError (FileError ())
  | GovernanceCommitteeHashCheckError !HashCheckError
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
    GovernanceCommitteeCmdScriptReadError e ->
      "Cannot read script file: " <> prettyError e
    GovernanceCommitteeHashCheckError hashCheckErr ->
      "Error while checking metadata hash: " <> pretty (displayException hashCheckErr)
