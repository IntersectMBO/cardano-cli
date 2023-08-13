module Cardano.CLI.EraBased.Errors.StakeAddress
  ( StakeAddressCmdError(..)
  , StakeAddressRegistrationError(..)
  , StakeAddressDelegationError(..)
  , renderStakeAddressCmdError
  ) where

import           Cardano.Api

import           Cardano.CLI.Read

import           Data.Text (Text)
import qualified Data.Text as Text

data StakeAddressCmdError
  = StakeAddressCmdReadKeyFileError !(FileError InputDecodeError)
  | StakeAddressCmdReadScriptFileError !(FileError ScriptDecodeError)
  | StakeAddressCmdWriteFileError !(FileError ())
  | StakeRegistrationError !StakeAddressRegistrationError
  | StakeDelegationError !StakeAddressDelegationError
  deriving Show

renderStakeAddressCmdError :: StakeAddressCmdError -> Text
renderStakeAddressCmdError err =
  case err of
    StakeAddressCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)
    StakeAddressCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    StakeAddressCmdReadScriptFileError fileErr -> Text.pack (displayError fileErr)
    StakeRegistrationError regErr -> Text.pack $ show regErr
    StakeDelegationError delegErr -> Text.pack $ show delegErr

data StakeAddressRegistrationError =
  StakeAddressRegistrationDepositRequiredError
  deriving Show

newtype StakeAddressDelegationError =
  VoteDelegationNotSupportedError AnyShelleyToBabbageEra
  deriving Show
