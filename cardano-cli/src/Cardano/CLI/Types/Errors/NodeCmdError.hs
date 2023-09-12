{-# LANGUAGE DataKinds #-}

module Cardano.CLI.Types.Errors.NodeCmdError
  ( NodeCmdError(..)
  , renderNodeCmdError
  ) where

import           Cardano.Api

import           Data.Text (Text)
import qualified Data.Text as Text

{- HLINT ignore "Reduce duplication" -}

data NodeCmdError
  = NodeCmdReadFileError !(FileError TextEnvelopeError)
  | NodeCmdReadKeyFileError !(FileError InputDecodeError)
  | NodeCmdWriteFileError !(FileError ())
  | NodeCmdOperationalCertificateIssueError !OperationalCertIssueError
  | NodeCmdVrfSigningKeyCreationError
      FilePath
      -- ^ Target path
      FilePath
      -- ^ Temp path
  deriving Show

renderNodeCmdError :: NodeCmdError -> Text
renderNodeCmdError err =
  case err of
    NodeCmdVrfSigningKeyCreationError targetPath tempPath ->
      Text.pack $ "Error creating VRF signing key file. Target path: " <> targetPath
      <> " Temporary path: " <> tempPath

    NodeCmdReadFileError fileErr -> Text.pack (displayError fileErr)

    NodeCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)

    NodeCmdWriteFileError fileErr -> Text.pack (displayError fileErr)

    NodeCmdOperationalCertificateIssueError issueErr ->
      Text.pack (displayError issueErr)
