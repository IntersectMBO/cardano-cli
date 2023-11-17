{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.StakeAddressCmdError
  ( StakeAddressCmdError(..)
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Errors.DelegationError
import           Cardano.CLI.Types.Errors.ScriptDecodeError
import           Cardano.CLI.Types.Errors.StakeAddressRegistrationError
import           Cardano.CLI.Types.Errors.StakeCredentialError

import           Prettyprinter

data StakeAddressCmdError
  = StakeAddressCmdReadKeyFileError !(FileError InputDecodeError)
  | StakeAddressCmdReadScriptFileError !(FileError ScriptDecodeError)
  | StakeAddressCmdStakeCredentialError !StakeCredentialError
  | StakeAddressCmdWriteFileError !(FileError ())
  | StakeAddressCmdDelegationError !DelegationError
  | StakeAddressCmdRegistrationError !StakeAddressRegistrationError
  deriving Show

instance Error StakeAddressCmdError where
  prettyError = \case
    StakeAddressCmdReadKeyFileError e       -> pretty e
    StakeAddressCmdReadScriptFileError e    -> pretty e
    StakeAddressCmdStakeCredentialError e   -> prettyError e
    StakeAddressCmdWriteFileError e         -> pretty e
    StakeAddressCmdDelegationError e        -> prettyError e
    StakeAddressCmdRegistrationError e      -> prettyError e
