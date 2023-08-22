{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.GovernanceVoteCmdError where

import           Cardano.Api.Shelley

import           Cardano.Binary (DecoderError)

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Formatting.Buildable as B

data GovernanceVoteCmdError
  = GovernanceVoteCmdReadError !(FileError InputDecodeError)
  | GovernanceVoteCmdCredentialDecodeError !DecoderError
  | GovernanceVoteCmdWriteError !(FileError ())
  deriving Show

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
