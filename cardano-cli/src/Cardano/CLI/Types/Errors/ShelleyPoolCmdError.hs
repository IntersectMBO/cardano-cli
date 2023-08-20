{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Types.Errors.ShelleyPoolCmdError
  ( ShelleyPoolCmdError(..)
  , renderShelleyPoolCmdError
  ) where

import           Cardano.Api

import           Data.Text (Text)
import qualified Data.Text as Text

data ShelleyPoolCmdError
  = ShelleyPoolCmdReadFileError !(FileError TextEnvelopeError)
  | ShelleyPoolCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyPoolCmdWriteFileError !(FileError ())
  | ShelleyPoolCmdMetadataValidationError !StakePoolMetadataValidationError
  deriving Show

renderShelleyPoolCmdError :: ShelleyPoolCmdError -> Text
renderShelleyPoolCmdError err =
  case err of
    ShelleyPoolCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyPoolCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyPoolCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyPoolCmdMetadataValidationError validationErr ->
      "Error validating stake pool metadata: " <> Text.pack (displayError validationErr)
