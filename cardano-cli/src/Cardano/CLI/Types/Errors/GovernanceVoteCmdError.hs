{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.GovernanceVoteCmdError where

import           Cardano.Api.Shelley

import           Cardano.Binary (DecoderError)
import           Cardano.CLI.Read (VoteError)

import qualified Data.Text.Lazy.Builder as TL
import qualified Formatting.Buildable as B
import           Prettyprinter

data GovernanceVoteCmdError
  = GovernanceVoteCmdReadVerificationKeyError !(FileError InputDecodeError)
  | GovernanceVoteCmdReadVoteFileError !VoteError
  | GovernanceVoteCmdCredentialDecodeError !DecoderError
  | GovernanceVoteCmdWriteError !(FileError ())
  | GovernanceVoteCmdReadVoteTextError !VoteError
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
    where
      renderDecoderError = pretty . TL.toLazyText . B.build
