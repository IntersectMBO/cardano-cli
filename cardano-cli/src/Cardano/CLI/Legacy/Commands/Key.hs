{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Commands.Key
  ( LegacyKeyCmds (..)
  , renderLegacyKeyCmds
  ) where

import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Common

import           Data.Text (Text)

data LegacyKeyCmds
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
  | KeyConvertByronGenesisVKeyCmd
      VerificationKeyBase64
      (File () Out)
  | KeyConvertITNKeyCmd
      (SomeKeyFile In)
      (File () Out)
  | KeyConvertITNExtendedKeyCmd
      (SomeKeyFile In) (File () Out)
  | KeyConvertITNBip32KeyCmd
      (SomeKeyFile In)
      (File () Out)
  | KeyConvertCardanoAddressKeyCmd
      CardanoAddressKeyType
      (SigningKeyFile In)
      (File () Out)
  deriving Show

renderLegacyKeyCmds :: LegacyKeyCmds -> Text
renderLegacyKeyCmds = \case
  KeyVerificationKeyCmd {} -> "key verification-key"
  KeyNonExtendedKeyCmd {} -> "key non-extended-key"
  KeyConvertByronKeyCmd {} -> "key convert-byron-key"
  KeyConvertByronGenesisVKeyCmd {} -> "key convert-byron-genesis-vkey"
  KeyConvertITNKeyCmd {} -> "key convert-itn-key"
  KeyConvertITNExtendedKeyCmd {} -> "key convert-itn-extended-key"
  KeyConvertITNBip32KeyCmd {} -> "key convert-itn-bip32-key"
  KeyConvertCardanoAddressKeyCmd {} -> "key convert-cardano-address-key"
