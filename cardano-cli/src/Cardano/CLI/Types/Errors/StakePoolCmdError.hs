{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Types.Errors.StakePoolCmdError
  ( StakePoolCmdError(..)
  , renderStakePoolCmdError
  ) where

import           Cardano.Api

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
    prettyError fileErr
  StakePoolCmdReadKeyFileError fileErr ->
    prettyError fileErr
  StakePoolCmdWriteFileError fileErr ->
    prettyError fileErr
