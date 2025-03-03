{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Type.Error.GovernanceVoteCmdError where

import Cardano.Api.Shelley

import Cardano.Binary (DecoderError)
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Read (VoteError)
import Cardano.CLI.Type.Error.HashCmdError (HashCheckError)

import Control.Exception (displayException)
import Data.Text.Lazy.Builder qualified as TL
import Formatting.Buildable qualified as B

data GovernanceVoteCmdError
  = GovernanceVoteCmdReadVerificationKeyError !(FileError InputDecodeError)
  | GovernanceVoteCmdReadVoteFileError !(FileError CliScriptWitnessError)
  | GovernanceVoteCmdCredentialDecodeError !DecoderError
  | GovernanceVoteCmdWriteError !(FileError ())
  | GovernanceVoteCmdReadVoteTextError !VoteError
  | GovernanceVoteCmdResignationCertHashCheckError !HashCheckError
  deriving Show

instance Error GovernanceVoteCmdError where
  prettyError = \case
    GovernanceVoteCmdReadVerificationKeyError e ->
      "Cannot read verification key: " <> prettyError e
    GovernanceVoteCmdReadVoteFileError e ->
      "Cannot read vote file: " <> prettyError e
    GovernanceVoteCmdCredentialDecodeError e ->
      "Cannot decode voting credential: " <> renderDecoderError e
    GovernanceVoteCmdWriteError e ->
      "Cannot write vote: " <> prettyError e
    GovernanceVoteCmdReadVoteTextError e ->
      "Cannot read vote text: " <> prettyError e
    GovernanceVoteCmdResignationCertHashCheckError hashCheckErr ->
      "Error while checking resignation certificate metadata hash: "
        <> pretty (displayException hashCheckErr)
   where
    renderDecoderError = pretty . TL.toLazyText . B.build
