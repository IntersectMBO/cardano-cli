{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.StakeCredentialError
  ( StakeCredentialError (..)
  )
where

import           Cardano.Api

import           Cardano.CLI.Types.Errors.ScriptDecodeError

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
