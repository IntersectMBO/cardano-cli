{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Type.Error.AddressCmdError
  ( AddressCmdError (..)
  , renderAddressCmdError
  )
where

import Cardano.Api

import Cardano.CLI.Read

data AddressCmdError
  = AddressCmdReadKeyFileError !(FileError InputDecodeError)
  | AddressCmdReadScriptFileError !(FileError ScriptDecodeError)
  | AddressCmdExpectedPaymentVerificationKey SomeAddressVerificationKey
  deriving Show

instance Error AddressCmdError where
  prettyError = renderAddressCmdError

renderAddressCmdError :: AddressCmdError -> Doc ann
renderAddressCmdError = \case
  AddressCmdReadKeyFileError fileErr ->
    prettyError fileErr
  AddressCmdReadScriptFileError fileErr ->
    prettyError fileErr
  AddressCmdExpectedPaymentVerificationKey someAddress ->
    "Expected payment verification key but got: "
      <> pretty (renderSomeAddressVerificationKey someAddress)
