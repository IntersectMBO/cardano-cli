{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Key
  ( KeyCmds (..)
  , KeyVerificationKeyCmdArgs(..)
  , KeyNonExtendedKeyCmdArgs(..)
  , KeyConvertByronKeyCmdArgs(..)
  , KeyConvertByronGenesisKeyCmdArgs(..)
  , KeyConvertITNKeyCmdArgs(..)
  , KeyConvertITNExtendedKeyCmdArgs(..)
  , KeyConvertITNBip32KeyCmdArgs(..)
  , KeyConvertCardanoAddressKeyCmdArgs(..)
  , renderKeyCmds
  ) where

import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Common

import           Data.Text (Text)

data KeyCmds era
  = KeyVerificationKeyCmd           !KeyVerificationKeyCmdArgs
  | KeyNonExtendedKeyCmd            !KeyNonExtendedKeyCmdArgs
  | KeyConvertByronKeyCmd           !KeyConvertByronKeyCmdArgs
  | KeyConvertByronGenesisKeyCmd    !KeyConvertByronGenesisKeyCmdArgs
  | KeyConvertITNKeyCmd             !KeyConvertITNKeyCmdArgs
  | KeyConvertITNExtendedKeyCmd     !KeyConvertITNExtendedKeyCmdArgs
  | KeyConvertITNBip32KeyCmd        !KeyConvertITNBip32KeyCmdArgs
  | KeyConvertCardanoAddressKeyCmd  !KeyConvertCardanoAddressKeyCmdArgs
  deriving Show

data KeyVerificationKeyCmdArgs = KeyVerificationKeyCmdArgs
  { skeyFile  :: !(SigningKeyFile In)
  , vkeyFile  :: !(VerificationKeyFile Out)
  } deriving Show

data KeyNonExtendedKeyCmdArgs = KeyNonExtendedKeyCmdArgs
  { extendedVkeyFileIn      :: !(VerificationKeyFile In)
  , nonExtendedVkeyFileOut  :: !(VerificationKeyFile Out)
  } deriving Show

data KeyConvertByronKeyCmdArgs = KeyConvertByronKeyCmdArgs
  { mPassword       :: !(Maybe Text)      -- ^ Password (if applicable)
  , byronKeyType    :: !ByronKeyType
  , someKeyFileIn   :: !(SomeKeyFile In)  -- ^ Input file: old format
  , someKeyFileOut  :: !(File () Out)     -- ^ Output file: new format
  } deriving Show

data KeyConvertByronGenesisKeyCmdArgs = KeyConvertByronGenesisKeyCmdArgs
  { vkey        :: VerificationKeyBase64  -- ^ Input key raw old format
  , vkeyFileOut :: !(File () Out)         -- ^ Output file: new format
  } deriving Show

data KeyConvertITNKeyCmdArgs = KeyConvertITNKeyCmdArgs
  { itnKeyFile  :: !(SomeKeyFile In)
  , outFile     :: !(File () Out)
  } deriving Show

data KeyConvertITNExtendedKeyCmdArgs = KeyConvertITNExtendedKeyCmdArgs
  { itnPrivKeyFile  :: !(SomeKeyFile In)
  , outFile         :: !(File () Out)
  } deriving Show

data KeyConvertITNBip32KeyCmdArgs = KeyConvertITNBip32KeyCmdArgs
  { itnPrivKeyFile  :: !(SomeKeyFile In)
  , outFile         :: !(File () Out)
  } deriving Show

data KeyConvertCardanoAddressKeyCmdArgs = KeyConvertCardanoAddressKeyCmdArgs
  { cardanoAddressKeyType :: !CardanoAddressKeyType
  , skeyFileIn            :: !(SigningKeyFile In)
  , skeyFileOut           :: !(File () Out)
  } deriving Show

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
