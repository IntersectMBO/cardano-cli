{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Vote where

import           Cardano.Api.Shelley

import           Cardano.Binary (DecoderError)

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Formatting.Buildable as B

data EraBasedVoteError
  = EraBasedVoteReadError !(FileError InputDecodeError)
  | EraBasedVotingCredentialDecodeError !DecoderError
  | EraBasedVoteWriteError !(FileError ())
  deriving Show

instance Error EraBasedVoteError where
  displayError = \case
    EraBasedVoteReadError e ->
      "Cannot read verification key: " <> displayError e
    EraBasedVotingCredentialDecodeError e ->
      "Cannot decode voting credential: " <> renderDecoderError e
    EraBasedVoteWriteError e ->
      "Cannot write vote: " <> displayError e
    where
      renderDecoderError = TL.unpack . TL.toLazyText . B.build
