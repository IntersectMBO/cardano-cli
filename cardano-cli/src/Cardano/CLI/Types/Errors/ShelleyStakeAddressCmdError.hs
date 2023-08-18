{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.ShelleyStakeAddressCmdError
  ( ShelleyStakeAddressCmdError(..)
  ) where

import           Cardano.Api

import           Cardano.CLI.Read
import           Cardano.CLI.Types.Errors.StakeAddressDelegationError
import           Cardano.CLI.Types.Errors.StakeAddressRegistrationError

data ShelleyStakeAddressCmdError
  = ShelleyStakeAddressCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyStakeAddressCmdReadScriptFileError !(FileError ScriptDecodeError)
  | ShelleyStakeAddressCmdWriteFileError !(FileError ())
  | StakeRegistrationError !StakeAddressRegistrationError
  | StakeDelegationError !StakeAddressDelegationError
  deriving Show

instance Error ShelleyStakeAddressCmdError where
  displayError = \case
    ShelleyStakeAddressCmdReadKeyFileError fileErr    -> displayError fileErr
    ShelleyStakeAddressCmdWriteFileError fileErr      -> displayError fileErr
    ShelleyStakeAddressCmdReadScriptFileError fileErr -> displayError fileErr
    StakeRegistrationError regErr                     -> displayError regErr
    StakeDelegationError delegErr                     -> displayError delegErr
