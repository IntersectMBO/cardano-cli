{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Types.Errors.StakePoolCmdError
  ( StakePoolCmdError(..)
  , renderStakePoolCmdError
  ) where

import           Cardano.Api
import           Cardano.Api.Pretty

import           Prettyprinter

data StakePoolCmdError
  = StakePoolCmdReadFileError !(FileError TextEnvelopeError)
  | StakePoolCmdReadKeyFileError !(FileError InputDecodeError)
  | StakePoolCmdWriteFileError !(FileError ())
  | StakePoolCmdMetadataValidationError !StakePoolMetadataValidationError
  deriving Show

renderStakePoolCmdError :: StakePoolCmdError -> Doc ann
renderStakePoolCmdError = \case
  StakePoolCmdMetadataValidationError validationErr ->
    "Error validating stake pool metadata: " <> prettyError validationErr
  StakePoolCmdReadFileError fileErr ->
    pretty fileErr
  StakePoolCmdReadKeyFileError fileErr ->
    pretty fileErr
  StakePoolCmdWriteFileError fileErr ->
    pretty fileErr
