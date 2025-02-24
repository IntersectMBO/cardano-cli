{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Type.Error.StakeAddressCmdError
  ( StakeAddressCmdError (..)
  )
where

import Cardano.Api

import Cardano.CLI.Type.Error.DelegationError
import Cardano.CLI.Type.Error.ScriptDecodeError
import Cardano.CLI.Type.Error.StakeAddressRegistrationError
import Cardano.CLI.Type.Error.StakeCredentialError

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
    StakeAddressCmdReadKeyFileError e -> prettyError e
    StakeAddressCmdReadScriptFileError e -> prettyError e
    StakeAddressCmdStakeCredentialError e -> prettyError e
    StakeAddressCmdWriteFileError e -> prettyError e
    StakeAddressCmdDelegationError e -> prettyError e
    StakeAddressCmdRegistrationError e -> prettyError e
