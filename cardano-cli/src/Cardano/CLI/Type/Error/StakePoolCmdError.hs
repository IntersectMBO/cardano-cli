{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Type.Error.StakePoolCmdError
  ( StakePoolCmdError (..)
  )
where

import Cardano.Api
import Cardano.Api.Experimental.Certificate
  ( Hash (StakePoolMetadataHash)
  , StakePoolMetadata
  , StakePoolMetadataValidationError
  , StakePoolRelay
  )

import Cardano.CLI.Type.Error.HashCmdError (FetchURLError)
import Cardano.Network.Ping (PingException)

import Control.Exception (displayException)
import Prettyprinter qualified as PP

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
  | StakePoolCmdRelayPingErrors ![PingException]
  | StakePoolCmdInvalidRelayError !StakePoolRelay
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
    StakePoolCmdRelayPingErrors errs ->
      PP.vsep
        [ "Errors validating stake pool relays:"
        , PP.indent 2 $ PP.vsep (PP.pretty . displayException <$> errs)
        ]
    StakePoolCmdInvalidRelayError relay ->
      PP.vsep
        [ "Cannot validate stake pool relay:" <+> pretty (show relay)
        , "A relay given by IP address must specify a port and at least one of an"
            <+> "IPv4 or IPv6 address."
        ]
