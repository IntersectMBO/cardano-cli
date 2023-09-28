{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.GovernanceVoteCmdError where

import Cardano.Api.Shelley

import Cardano.Binary (DecoderError)

import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TL
import Formatting.Buildable qualified as B

data GovernanceVoteCmdError
  = GovernanceVoteCmdReadError !(FileError InputDecodeError)
  | GovernanceVoteCmdCredentialDecodeError !DecoderError
  | GovernanceVoteCmdWriteError !(FileError ())
  deriving (Show)

instance Error GovernanceVoteCmdError where
  displayError = \case
    GovernanceVoteCmdReadError e ->
      "Cannot read verification key: " <> displayError e
    GovernanceVoteCmdCredentialDecodeError e ->
      "Cannot decode voting credential: " <> renderDecoderError e
    GovernanceVoteCmdWriteError e ->
      "Cannot write vote: " <> displayError e
   where
    renderDecoderError = TL.unpack . TL.toLazyText . B.build
