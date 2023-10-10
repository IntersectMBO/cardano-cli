{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Key
  ( KeyCmds (..)
  , renderKeyCmds
  ) where

import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Common

import           Data.Text (Text)

data KeyCmds era
  = KeyVerificationKeyCmd
      (SigningKeyFile In)
      (VerificationKeyFile Out)
  | KeyNonExtendedKeyCmd
      (VerificationKeyFile In)
      (VerificationKeyFile Out)
  | KeyConvertByronKeyCmd
      (Maybe Text)
      ByronKeyType
      (SomeKeyFile In)
      (File () Out)
  | KeyConvertByronGenesisKeyCmd
      VerificationKeyBase64
      (File () Out)
  | KeyConvertITNKeyCmd
      (SomeKeyFile In)
      (File () Out)
  | KeyConvertITNExtendedKeyCmd
      (SomeKeyFile In)
      (File () Out)
  | KeyConvertITNBip32KeyCmd
      (SomeKeyFile In)
      (File () Out)
  | KeyConvertCardanoAddressKeyCmd
      CardanoAddressKeyType
      (SigningKeyFile In)
      (File () Out)
  deriving Show

renderKeyCmds :: KeyCmds era -> Text
renderKeyCmds = \case
  KeyVerificationKeyCmd {} ->
    "key verification-key"
  KeyNonExtendedKeyCmd {} ->
    "key non-extended-key"
  KeyConvertByronKeyCmd {} ->
    "key convert-byron-key"
  KeyConvertByronGenesisKeyCmd {} ->
    "key convert-byron-genesis-key"
  KeyConvertITNKeyCmd {} ->
    "key convert-itn-key"
  KeyConvertITNExtendedKeyCmd {} ->
    "key convert-itn-extended-key"
  KeyConvertITNBip32KeyCmd {} ->
    "key convert-itn-bip32-key"
  KeyConvertCardanoAddressKeyCmd {} ->
    "key convert-cardano-address-key"
