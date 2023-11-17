{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.NodeCmdError
  ( NodeCmdError(..)
  , renderNodeCmdError
  ) where

import           Cardano.Api
import           Cardano.Api.Pretty

import           Prettyprinter

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

renderNodeCmdError :: NodeCmdError -> Doc ann
renderNodeCmdError = \case
    NodeCmdVrfSigningKeyCreationError targetPath tempPath ->
      "Error creating VRF signing key file. Target path: " <> pshow targetPath
      <> " Temporary path: " <> pshow tempPath
    NodeCmdReadFileError fileErr ->
      pretty fileErr
    NodeCmdReadKeyFileError fileErr ->
      pretty fileErr
    NodeCmdWriteFileError fileErr ->
      pretty fileErr
    NodeCmdOperationalCertificateIssueError issueErr ->
      prettyError issueErr
