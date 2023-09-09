{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.StakeAddressCmdError
  ( StakeAddressCmdError(..)
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Errors.ScriptDecodeError
import           Cardano.CLI.Types.Errors.StakeAddressRegistrationError
import           Cardano.CLI.Types.Errors.StakeCredentialError
import Cardano.CLI.Types.Errors.DelegationError

data StakeAddressCmdError
  = StakeAddressCmdReadKeyFileError !(FileError InputDecodeError)
  | StakeAddressCmdReadScriptFileError !(FileError ScriptDecodeError)
  | StakeAddressCmdStakeCredentialError !StakeCredentialError
  | StakeAddressCmdWriteFileError !(FileError ())
  | StakeAddressCmdDelegationError !DelegationError
  | StakeAddressCmdRegistrationError !StakeAddressRegistrationError
  deriving Show

instance Error StakeAddressCmdError where
  displayError = \case
    StakeAddressCmdReadKeyFileError e       -> displayError e
    StakeAddressCmdReadScriptFileError e    -> displayError e
    StakeAddressCmdStakeCredentialError e   -> displayError e
    StakeAddressCmdWriteFileError e         -> displayError e
    StakeAddressCmdDelegationError e        -> displayError e
    StakeAddressCmdRegistrationError e      -> displayError e
