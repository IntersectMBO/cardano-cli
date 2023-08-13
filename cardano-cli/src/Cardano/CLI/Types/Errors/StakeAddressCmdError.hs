module Cardano.CLI.Types.Errors.StakeAddressCmdError
  ( StakeAddressCmdError(..)
  , renderStakeAddressCmdError
  ) where

import           Cardano.Api

import           Cardano.CLI.Read
import           Cardano.CLI.Types.Errors.StakeAddressDelegationError
import           Cardano.CLI.Types.Errors.StakeAddressRegistrationError

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
