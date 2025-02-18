{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Types.Errors.StakePoolCmdError
  ( StakePoolCmdError (..)
  , renderStakePoolCmdError
  )
where

import Cardano.Api
import Cardano.Api.Shelley (Hash (StakePoolMetadataHash))

import Cardano.CLI.Types.Errors.HashCmdError (FetchURLError)

data StakePoolCmdError
  = StakePoolCmdReadFileError !(FileError TextEnvelopeError)
  | StakePoolCmdReadKeyFileError !(FileError InputDecodeError)
  | StakePoolCmdWriteFileError !(FileError ())
  | StakePoolCmdMetadataValidationError !StakePoolMetadataValidationError
  | StakePoolCmdHashMismatchError
      !(Hash StakePoolMetadata)
      -- ^ Expected hash
      !(Hash StakePoolMetadata)
      -- ^ Actual hash
  | StakePoolCmdFetchURLError !FetchURLError
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
