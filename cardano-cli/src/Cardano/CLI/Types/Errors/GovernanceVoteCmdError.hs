{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.GovernanceVoteCmdError where

import           Cardano.Api.Shelley

import           Cardano.Binary (DecoderError)
import           Cardano.CLI.EraBased.Script.Types
import           Cardano.CLI.Read (VoteError)
import           Cardano.CLI.Types.Errors.HashCmdError (HashCheckError)

import           Control.Exception (displayException)
import qualified Data.Text.Lazy.Builder as TL
import qualified Formatting.Buildable as B

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
