{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.GovernanceVoteCmdError where

import           Cardano.Api.Shelley

import           Cardano.Binary (DecoderError)
import           Cardano.CLI.Read (VoteError)

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Formatting.Buildable as B

data GovernanceVoteCmdError
  = GovernanceVoteCmdReadVerificationKeyError !(FileError InputDecodeError)
  | GovernanceVoteCmdReadVoteFileError !VoteError
  | GovernanceVoteCmdCredentialDecodeError !DecoderError
  | GovernanceVoteCmdWriteError !(FileError ())
  | GovernanceVoteCmdReadVoteTextError !VoteError
  deriving Show

instance Error GovernanceVoteCmdError where
  displayError = \case
    GovernanceVoteCmdReadVerificationKeyError e ->
      "Cannot read verification key: " <> displayError e
    GovernanceVoteCmdReadVoteFileError e ->
      "Cannot read vote file: " <> displayError e
    GovernanceVoteCmdCredentialDecodeError e ->
      "Cannot decode voting credential: " <> renderDecoderError e
    GovernanceVoteCmdWriteError e ->
      "Cannot write vote: " <> displayError e
    GovernanceVoteCmdReadVoteTextError e ->
      "Cannot read vote text: " <> displayError e
    where
      renderDecoderError = TL.unpack . TL.toLazyText . B.build
