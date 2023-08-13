
module Cardano.CLI.Types.Errors.EraBasedRegistrationError
  ( EraBasedRegistrationError(..)
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Errors.StakeAddressCmdError
import           Cardano.CLI.Types.Errors.StakeAddressRegistrationError

data EraBasedRegistrationError
  = EraBasedRegistReadError !(FileError InputDecodeError)
  | EraBasedRegistWriteFileError !(FileError ())
  | EraBasedRegistStakeCredReadError !StakeAddressCmdError -- TODO: Conway era - don't use legacy error type
  | EraBasedRegistStakeError StakeAddressRegistrationError
