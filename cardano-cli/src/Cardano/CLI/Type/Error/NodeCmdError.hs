{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Type.Error.NodeCmdError
  ( NodeCmdError (..)
  , renderNodeCmdError
  )
where

import Cardano.Api

newtype NodeCmdError
  = NodeCmdReadFileError (FileError TextEnvelopeError)
  deriving Show

instance Error NodeCmdError where
  prettyError :: NodeCmdError -> Doc ann
  prettyError = renderNodeCmdError

renderNodeCmdError :: NodeCmdError -> Doc ann
renderNodeCmdError = \case
  NodeCmdReadFileError fileErr ->
    prettyError fileErr
