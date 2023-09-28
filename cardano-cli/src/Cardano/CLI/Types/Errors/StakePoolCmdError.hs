{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Types.Errors.StakePoolCmdError
  ( StakePoolCmdError (..)
  , renderStakePoolCmdError
  ) where

import Cardano.Api

import Data.Text (Text)
import qualified Data.Text as Text

data StakePoolCmdError
  = StakePoolCmdReadFileError !(FileError TextEnvelopeError)
  | StakePoolCmdReadKeyFileError !(FileError InputDecodeError)
  | StakePoolCmdWriteFileError !(FileError ())
  | StakePoolCmdMetadataValidationError !StakePoolMetadataValidationError
  deriving (Show)

renderStakePoolCmdError :: StakePoolCmdError -> Text
renderStakePoolCmdError = \case
  StakePoolCmdMetadataValidationError validationErr ->
    "Error validating stake pool metadata: " <> Text.pack (displayError validationErr)
  StakePoolCmdReadFileError fileErr ->
    Text.pack (displayError fileErr)
  StakePoolCmdReadKeyFileError fileErr ->
    Text.pack (displayError fileErr)
  StakePoolCmdWriteFileError fileErr ->
    Text.pack (displayError fileErr)
