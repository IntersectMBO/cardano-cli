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

newtype AddressCmdError
  = AddressCmdExpectedPaymentVerificationKey SomeAddressVerificationKey
  deriving Show

instance Error AddressCmdError where
  prettyError = renderAddressCmdError

renderAddressCmdError :: AddressCmdError -> Doc ann
renderAddressCmdError = \case
  AddressCmdExpectedPaymentVerificationKey someAddress ->
    "Expected payment verification key but got: "
      <> pretty (renderSomeAddressVerificationKey someAddress)
