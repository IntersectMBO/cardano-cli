{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Type.Error.StakePoolCmdError
  ( StakePoolCmdError (..)
  )
where

import Cardano.Api

import Cardano.CLI.Type.Error.HashCmdError (FetchURLError)

data StakePoolCmdError
  = StakePoolCmdReadFileError !(FileError TextEnvelopeError)
  | StakePoolCmdWriteFileError !(FileError ())
  | StakePoolCmdMetadataValidationError !StakePoolMetadataValidationError
  | StakePoolCmdHashMismatchError
      !(Hash StakePoolMetadata)
      -- ^ Expected hash
      !(Hash StakePoolMetadata)
      -- ^ Actual hash
  | StakePoolCmdFetchURLError !FetchURLError
  deriving Show

instance Error StakePoolCmdError where
  prettyError = \case
    StakePoolCmdMetadataValidationError validationErr ->
      "Error validating stake pool metadata: " <> prettyError validationErr
    StakePoolCmdReadFileError fileErr ->
      prettyError fileErr
    StakePoolCmdWriteFileError fileErr ->
      prettyError fileErr
    StakePoolCmdHashMismatchError
      (StakePoolMetadataHash expectedHash)
      (StakePoolMetadataHash actualHash) ->
        "Hashes do not match!"
          <> "\nExpected:"
            <+> pretty (show expectedHash)
          <> "\n  Actual:"
            <+> pretty (show actualHash)
    StakePoolCmdFetchURLError fetchErr ->
      "Error fetching stake pool metadata: " <> prettyException fetchErr
