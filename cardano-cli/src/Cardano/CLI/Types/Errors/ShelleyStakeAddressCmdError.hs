{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.ShelleyStakeAddressCmdError
  ( ShelleyStakeAddressCmdError(..)
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Errors.ScriptDecodeError
import           Cardano.CLI.Types.Errors.StakeAddressRegistrationError
import           Cardano.CLI.Types.Errors.StakeCredentialError

data ShelleyStakeAddressCmdError
  = ShelleyStakeAddressCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyStakeAddressCmdReadScriptFileError !(FileError ScriptDecodeError)
  | ShelleyStakeAddressCmdStakeCredentialError !StakeCredentialError
  | ShelleyStakeAddressCmdWriteFileError !(FileError ())
  | StakeRegistrationError !StakeAddressRegistrationError
  deriving Show

instance Error ShelleyStakeAddressCmdError where
  displayError = \case
    ShelleyStakeAddressCmdReadKeyFileError e      -> displayError e
    ShelleyStakeAddressCmdReadScriptFileError e   -> displayError e
    ShelleyStakeAddressCmdStakeCredentialError e  -> displayError e
    ShelleyStakeAddressCmdWriteFileError e        -> displayError e
    StakeRegistrationError e                      -> displayError e
