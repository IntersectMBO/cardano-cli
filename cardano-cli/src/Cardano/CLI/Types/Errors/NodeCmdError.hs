{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.CLI.Types.Errors.NodeCmdError
  ( NodeCmdError(..)
  , renderNodeCmdError
  ) where

import           Cardano.Api

import           Data.Text (Text)
import qualified Data.Text as Text

data NodeCmdError
  = NodeCmdReadFileError
      !(FileError TextEnvelopeError)
  | NodeCmdReadKeyFileError
      !(FileError InputDecodeError)
  | NodeCmdWriteFileError
      !(FileError ())
  | NodeCmdOperationalCertificateIssueError
      !OperationalCertIssueError
  | NodeCmdVrfSigningKeyCreationError
      FilePath
      -- ^ Target path
      FilePath
      -- ^ Temp path
  deriving Show

renderNodeCmdError :: NodeCmdError -> Text
renderNodeCmdError = \case
  NodeCmdVrfSigningKeyCreationError targetPath tempPath ->
    mconcat
      [ "Error creating VRF signing key file. Target path: " <> Text.pack targetPath
      , " Temporary path: " <> Text.pack tempPath
      ]

  NodeCmdReadFileError fileErr ->
    Text.pack (displayError fileErr)

  NodeCmdReadKeyFileError fileErr ->
    Text.pack (displayError fileErr)

  NodeCmdWriteFileError fileErr ->
    Text.pack (displayError fileErr)

  NodeCmdOperationalCertificateIssueError issueErr ->
    Text.pack (displayError issueErr)
