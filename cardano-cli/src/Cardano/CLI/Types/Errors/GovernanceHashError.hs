module Cardano.CLI.Types.Errors.GovernanceHashError
  ( GovernanceHashError(..)
  ) where

import           Cardano.Api

import           Cardano.Prelude (UnicodeException)

data GovernanceHashError
  = GovernanceHashReadFileError (FileError InputDecodeError)
  | GovernanceHashUnicodeError UnicodeException
  deriving Show

instance Error GovernanceHashError where
  displayError = undefined
