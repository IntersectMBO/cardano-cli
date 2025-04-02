{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Type.Error.RegistrationError
  ( RegistrationError (..)
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Type.Error.HashCmdError (FetchURLError, HashCheckError)
import Cardano.CLI.Type.Error.StakeAddressRegistrationError
import Cardano.CLI.Type.Error.StakeCredentialError

import Control.Exception (displayException)

data RegistrationError
  = RegistrationReadError !(FileError InputDecodeError)
  | RegistrationWriteFileError !(FileError ())
  | RegistrationStakeCredentialError !StakeCredentialError
  | RegistrationStakeError !StakeAddressRegistrationError
  | RegistrationMismatchedDRepMetadataHashError
      !(L.SafeHash L.AnchorData)
      -- ^ The expected DRep metadata hash.
      !(L.SafeHash L.AnchorData)
      -- ^ The actual DRep metadata hash.
  | RegistrationFetchURLError !FetchURLError
  | RegistrationDRepHashCheckError !HashCheckError
  deriving Show

instance Error RegistrationError where
  prettyError = \case
    RegistrationReadError e ->
      "Cannot read registration certificate: " <> prettyError e
    RegistrationWriteFileError e ->
      "Cannot write registration certificate: " <> prettyError e
    RegistrationStakeCredentialError e ->
      "Cannot read stake credential: " <> prettyError e
    RegistrationStakeError e ->
      "Stake address registation error: " <> prettyError e
    RegistrationMismatchedDRepMetadataHashError expectedHash actualHash ->
      "DRep metadata Hashes do not match!"
        <> "\nExpected:"
          <+> pretty (show (L.extractHash expectedHash))
        <> "\n  Actual:"
          <+> pretty (show (L.extractHash actualHash))
    RegistrationFetchURLError fetchErr ->
      "Error while fetching proposal: " <> pretty (displayException fetchErr)
    RegistrationDRepHashCheckError hashCheckError ->
      "Error while checking DRep metadata hash: " <> pretty (displayException hashCheckError)
