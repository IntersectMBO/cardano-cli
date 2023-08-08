module Cardano.CLI.EraBased.Errors.StakeAddress
  ( ShelleyStakeAddressCmdError(..)
  , StakeAddressRegistrationError(..)
  , StakeAddressDelegationError(..)
  , renderShelleyStakeAddressCmdError
  ) where

import           Cardano.Api

import           Cardano.CLI.Legacy.Run.Read

import           Data.Text (Text)
import qualified Data.Text as Text

data ShelleyStakeAddressCmdError
  = ShelleyStakeAddressCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyStakeAddressCmdReadScriptFileError !(FileError ScriptDecodeError)
  | ShelleyStakeAddressCmdWriteFileError !(FileError ())
  | StakeRegistrationError !StakeAddressRegistrationError
  | StakeDelegationError !StakeAddressDelegationError
  deriving Show

renderShelleyStakeAddressCmdError :: ShelleyStakeAddressCmdError -> Text
renderShelleyStakeAddressCmdError err =
  case err of
    ShelleyStakeAddressCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyStakeAddressCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyStakeAddressCmdReadScriptFileError fileErr -> Text.pack (displayError fileErr)
    StakeRegistrationError regErr -> Text.pack $ show regErr
    StakeDelegationError delegErr -> Text.pack $ show delegErr

data StakeAddressRegistrationError = StakeAddressRegistrationDepositRequired deriving Show

newtype StakeAddressDelegationError = VoteDelegationNotSupported AnyShelleyToBabbageEra deriving Show
