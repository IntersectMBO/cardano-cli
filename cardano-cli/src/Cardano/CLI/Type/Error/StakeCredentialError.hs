{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Type.Error.StakeCredentialError
  ( StakeCredentialError (..)
  )
where

import Cardano.Api

import Cardano.CLI.Type.Error.ScriptDecodeError

data StakeCredentialError
  = StakeCredentialScriptDecodeError (FileError ScriptDecodeError)
  | StakeCredentialInputDecodeError (FileError InputDecodeError)
  deriving Show

instance Error StakeCredentialError where
  prettyError = \case
    StakeCredentialScriptDecodeError e ->
      prettyError e
    StakeCredentialInputDecodeError e ->
      prettyError e
