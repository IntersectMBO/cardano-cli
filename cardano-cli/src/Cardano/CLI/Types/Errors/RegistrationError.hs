{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.RegistrationError
  ( RegistrationError(..)
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Errors.StakeAddressRegistrationError
import           Cardano.CLI.Types.Errors.StakeCredentialError

import           Prettyprinter

data RegistrationError
  = RegistrationReadError !(FileError InputDecodeError)
  | RegistrationWriteFileError !(FileError ())
  | RegistrationStakeCredentialError !StakeCredentialError
  | RegistrationStakeError !StakeAddressRegistrationError
  deriving Show

instance Error RegistrationError where
  prettyError = \case
    RegistrationReadError e ->
      "Cannot read registration certificate: " <> pretty e
    RegistrationWriteFileError e ->
      "Cannot write registration certificate: " <> pretty e
    RegistrationStakeCredentialError e ->
      "Cannot read stake credential: " <> prettyError e
    RegistrationStakeError e ->
      "Stake address registation error: " <> prettyError e
