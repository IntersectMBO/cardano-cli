module Cardano.CLI.Types.Errors.EraBasedDelegationError
  ( EraBasedDelegationError(..)

  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Errors.StakeAddressCmdError

data EraBasedDelegationError
  = EraBasedDelegReadError
      !(FileError InputDecodeError)
  | EraBasedCredentialError
      !StakeAddressCmdError -- TODO: Refactor. We shouldn't be using legacy error types
  | EraBasedCertificateWriteFileError
      !(FileError ())
  | EraBasedDRepReadError
      !(FileError InputDecodeError)
  | EraBasedDelegationGenericError -- TODO Delete and replace with more specific errors
