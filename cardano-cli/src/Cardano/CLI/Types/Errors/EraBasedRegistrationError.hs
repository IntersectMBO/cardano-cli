{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.EraBasedRegistrationError
  ( EraBasedRegistrationError(..)
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Errors.StakeAddressRegistrationError
import           Cardano.CLI.Types.Errors.StakeCredentialError

data EraBasedRegistrationError
  = EraBasedRegistReadError !(FileError InputDecodeError)
  | EraBasedRegistWriteFileError !(FileError ())
  | EraBasedRegistrationStakeCredentialError !StakeCredentialError
  | EraBasedRegistStakeError !StakeAddressRegistrationError
  deriving Show

instance Error EraBasedRegistrationError where
  displayError = \case
    EraBasedRegistReadError e ->
      "Cannot read registration certificate: " <> displayError e
    EraBasedRegistWriteFileError e ->
      "Cannot write registration certificate: " <> displayError e
    EraBasedRegistrationStakeCredentialError e ->
      "Cannot read stake credential: " <> displayError e
    EraBasedRegistStakeError e ->
      "Stake address registation error: " <> displayError e
