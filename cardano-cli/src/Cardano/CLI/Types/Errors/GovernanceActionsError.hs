
module Cardano.CLI.Types.Errors.GovernanceActionsError
  ( GovernanceActionsError(..)
  ) where

import           Cardano.Api

import           Data.Text.Encoding.Error

data GovernanceActionsError
  = GovernanceActionsCmdWriteFileError
      (FileError ())
  | GovernanceActionsCmdReadFileError
      (FileError InputDecodeError)
  | GovernanceActionsCmdNonUtf8EncodedConstitution
      UnicodeException
