{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Types.Errors.StakePoolCmdError
  ( StakePoolCmdError (..)
  , renderStakePoolCmdError
  )
where

import           Cardano.Api

import           Cardano.CLI.Types.Errors.HashCmdError (HashCheckError)

data StakePoolCmdError
  = StakePoolCmdReadFileError !(FileError TextEnvelopeError)
  | StakePoolCmdReadKeyFileError !(FileError InputDecodeError)
  | StakePoolCmdWriteFileError !(FileError ())
  | StakePoolCmdMetadataValidationError !StakePoolMetadataValidationError
  | StakePoolCmdMetadataHashCheckError !HashCheckError
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
  StakePoolCmdMetadataHashCheckError hashCheckErr ->
    "Error checking stake pool metadata hash: " <> prettyException hashCheckErr
