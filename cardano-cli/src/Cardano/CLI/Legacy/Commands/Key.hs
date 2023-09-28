{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Commands.Key
  ( LegacyKeyCmds (..)
  , renderLegacyKeyCmds
  ) where

import Cardano.Api.Shelley

import Cardano.CLI.Types.Common

import Data.Text (Text)

data LegacyKeyCmds
  = KeyGetVerificationKey (SigningKeyFile In) (VerificationKeyFile Out)
  | KeyNonExtendedKey (VerificationKeyFile In) (VerificationKeyFile Out)
  | KeyConvertByronKey (Maybe Text) ByronKeyType (SomeKeyFile In) (File () Out)
  | KeyConvertByronGenesisVKey VerificationKeyBase64 (File () Out)
  | KeyConvertITNStakeKey (SomeKeyFile In) (File () Out)
  | KeyConvertITNExtendedToStakeKey (SomeKeyFile In) (File () Out)
  | KeyConvertITNBip32ToStakeKey (SomeKeyFile In) (File () Out)
  | KeyConvertCardanoAddressSigningKey CardanoAddressKeyType (SigningKeyFile In) (File () Out)
  deriving (Show)

renderLegacyKeyCmds :: LegacyKeyCmds -> Text
renderLegacyKeyCmds = \case
  KeyGetVerificationKey {} -> "key verification-key"
  KeyNonExtendedKey {} -> "key non-extended-key"
  KeyConvertByronKey {} -> "key convert-byron-key"
  KeyConvertByronGenesisVKey {} -> "key convert-byron-genesis-key"
  KeyConvertITNStakeKey {} -> "key convert-itn-key"
  KeyConvertITNExtendedToStakeKey {} -> "key convert-itn-extended-key"
  KeyConvertITNBip32ToStakeKey {} -> "key convert-itn-bip32-key"
  KeyConvertCardanoAddressSigningKey {} -> "key convert-cardano-address-signing-key"
