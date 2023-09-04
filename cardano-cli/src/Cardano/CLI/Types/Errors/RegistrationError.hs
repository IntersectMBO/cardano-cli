{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.RegistrationError
  ( RegistrationError(..)
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Errors.StakeAddressRegistrationError
import           Cardano.CLI.Types.Errors.StakeCredentialError

data RegistrationError
  = RegistrationReadError !(FileError InputDecodeError)
  | RegistrationWriteFileError !(FileError ())
  | RegistrationStakeCredentialError !StakeCredentialError
  | RegistrationStakeError !StakeAddressRegistrationError
  deriving Show

instance Error RegistrationError where
  displayError = \case
    RegistrationReadError e ->
      "Cannot read registration certificate: " <> displayError e
    RegistrationWriteFileError e ->
      "Cannot write registration certificate: " <> displayError e
    RegistrationStakeCredentialError e ->
      "Cannot read stake credential: " <> displayError e
    RegistrationStakeError e ->
      "Stake address registation error: " <> displayError e
