{-# LANGUAGE DataKinds #-}

module Cardano.CLI.Types.Errors.ShelleyNodeCmdError
  ( ShelleyNodeCmdError(..)
  , renderShelleyNodeCmdError
  ) where

import           Cardano.Api

import           Data.Text (Text)
import qualified Data.Text as Text

{- HLINT ignore "Reduce duplication" -}

data ShelleyNodeCmdError
  = ShelleyNodeCmdReadFileError !(FileError TextEnvelopeError)
  | ShelleyNodeCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyNodeCmdWriteFileError !(FileError ())
  | ShelleyNodeCmdOperationalCertificateIssueError !OperationalCertIssueError
  | ShelleyNodeCmdVrfSigningKeyCreationError
      FilePath
      -- ^ Target path
      FilePath
      -- ^ Temp path
  deriving Show

renderShelleyNodeCmdError :: ShelleyNodeCmdError -> Text
renderShelleyNodeCmdError err =
  case err of
    ShelleyNodeCmdVrfSigningKeyCreationError targetPath tempPath ->
      Text.pack $ "Error creating VRF signing key file. Target path: " <> targetPath
      <> " Temporary path: " <> tempPath

    ShelleyNodeCmdReadFileError fileErr -> Text.pack (displayError fileErr)

    ShelleyNodeCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)

    ShelleyNodeCmdWriteFileError fileErr -> Text.pack (displayError fileErr)

    ShelleyNodeCmdOperationalCertificateIssueError issueErr ->
      Text.pack (displayError issueErr)
