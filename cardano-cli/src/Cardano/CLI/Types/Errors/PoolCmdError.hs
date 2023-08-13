
module Cardano.CLI.Types.Errors.PoolCmdError
  ( PoolCmdError(..)
  , renderPoolCmdError
  ) where

import           Cardano.Api

import           Data.Text (Text)
import qualified Data.Text as Text

data PoolCmdError
  = PoolCmdReadFileError !(FileError TextEnvelopeError)
  | PoolCmdReadKeyFileError !(FileError InputDecodeError)
  | PoolCmdWriteFileError !(FileError ())
  | PoolCmdMetadataValidationError !StakePoolMetadataValidationError
  deriving Show

renderPoolCmdError :: PoolCmdError -> Text
renderPoolCmdError err =
  case err of
    PoolCmdReadFileError fileErr ->
      Text.pack (displayError fileErr)
    PoolCmdReadKeyFileError fileErr ->
      Text.pack (displayError fileErr)
    PoolCmdWriteFileError fileErr ->
      Text.pack (displayError fileErr)
    PoolCmdMetadataValidationError validationErr ->
      "Error validating stake pool metadata: " <> Text.pack (displayError validationErr)
