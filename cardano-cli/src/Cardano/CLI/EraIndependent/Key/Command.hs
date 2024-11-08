{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraIndependent.Key.Command
  ( KeyCmds (..)
  , KeyVerificationKeyCmdArgs (..)
  , KeyNonExtendedKeyCmdArgs (..)
  , KeyGenerateMnemonicCmdArgs (..)
  , KeyExtendedSigningKeyFromMnemonicArgs (..)
  , ExtendedSigningType (..)
  , MnemonicSource (..)
  , KeyConvertByronKeyCmdArgs (..)
  , KeyConvertByronGenesisVKeyCmdArgs (..)
  , KeyConvertITNKeyCmdArgs (..)
  , KeyConvertITNExtendedKeyCmdArgs (..)
  , KeyConvertITNBip32KeyCmdArgs (..)
  , KeyConvertCardanoAddressKeyCmdArgs (..)
  , renderKeyCmds
  )
where

import Cardano.Api.Shelley

import Cardano.CLI.Type.Common
import Cardano.Prelude (Word32)

import Data.Text (Text)

data KeyCmds
  = KeyVerificationKeyCmd !KeyVerificationKeyCmdArgs
  | KeyNonExtendedKeyCmd !KeyNonExtendedKeyCmdArgs
  | KeyGenerateMnemonicCmd !KeyGenerateMnemonicCmdArgs
  | KeyExtendedSigningKeyFromMnemonicCmd !KeyExtendedSigningKeyFromMnemonicArgs
  | KeyConvertByronKeyCmd !KeyConvertByronKeyCmdArgs
  | KeyConvertByronGenesisVKeyCmd !KeyConvertByronGenesisVKeyCmdArgs
  | KeyConvertITNKeyCmd !KeyConvertITNKeyCmdArgs
  | KeyConvertITNExtendedKeyCmd !KeyConvertITNExtendedKeyCmdArgs
  | KeyConvertITNBip32KeyCmd !KeyConvertITNBip32KeyCmdArgs
  | KeyConvertCardanoAddressKeyCmd !KeyConvertCardanoAddressKeyCmdArgs
  deriving Show

-- | Get a verification key from a signing key. This supports all key types
data KeyVerificationKeyCmdArgs = KeyVerificationKeyCmdArgs
  { skeyFile :: !(SigningKeyFile In)
  -- ^ Input filepath of the signing key
  , vkeyFile :: !(VerificationKeyFile Out)
  -- ^ Output filepath of the verification key
  }
  deriving Show

-- | Get a non-extended verification key from an extended verification key. This
-- supports all extended key types.
data KeyNonExtendedKeyCmdArgs = KeyNonExtendedKeyCmdArgs
  { extendedVkeyFileIn :: !(VerificationKeyFile In)
  -- ^ Input filepath of the ed25519-bip32 verification key
  , nonExtendedVkeyFileOut :: !(VerificationKeyFile Out)
  -- ^ Output filepath of the verification key
  }
  deriving Show

-- | Generate a mnemonic phrase that can be used to derive signing keys.
data KeyGenerateMnemonicCmdArgs = KeyGenerateMnemonicCmdArgs
  { mnemonicOutputFormat :: !(Maybe (File () Out))
  -- ^ Output format for the mnemonic phrase
  , mnemonicWords :: !MnemonicSize
  -- ^ Number of mnemonic words to generate it must be one of: 12, 15, 18, 21, or 24.
  }
  deriving Show

-- | Get an extended signing key from a mnemonic.
data KeyExtendedSigningKeyFromMnemonicArgs = KeyExtendedSigningKeyFromMnemonicArgs
  { keyOutputFormat :: !KeyOutputFormat
  , derivedExtendedSigningKeyType :: !ExtendedSigningType
  , derivationAccountNo :: !Word32
  , mnemonicSource :: !MnemonicSource
  , signingKeyFileOut :: !(SigningKeyFile Out)
  }
  deriving Show

data MnemonicSource
  = MnemonicFromFile !(File () In)
  | MnemonicFromInteractivePrompt
  deriving Show

-- | Type of the key derived from a mnemonic
-- together with the payment key number in the derivation path
-- for cases where it is applicable.
data ExtendedSigningType
  = ExtendedSigningPaymentKey !Word32
  | ExtendedSigningStakeKey !Word32
  | ExtendedSigningDRepKey
  | ExtendedSigningCCColdKey
  | ExtendedSigningCCHotKey
  deriving Show

-- | Convert a Byron payment, genesis or genesis delegate key (signing or
-- verification) to a corresponding Shelley-format key.
data KeyConvertByronKeyCmdArgs = KeyConvertByronKeyCmdArgs
  { mPassword :: !(Maybe Text)
  -- ^ Password for signing key (if applicable)
  , byronKeyType :: !ByronKeyType
  -- ^ The byron key type of the input file
  , someKeyFileIn :: !(SomeKeyFile In)
  -- ^ Input file containing the byron key
  , someKeyFileOut :: !(File () Out)
  -- ^ The output file to which the Shelley-format key will be written
  }
  deriving Show

-- Convert a Base64-encoded Byron genesis verification key to a Shelley genesis
-- verification key
data KeyConvertByronGenesisVKeyCmdArgs = KeyConvertByronGenesisVKeyCmdArgs
  { vkey :: !VerificationKeyBase64
  -- ^ Base64 string for the Byron genesis verification key
  , vkeyFileOut :: !(File () Out)
  -- ^ The output file
  }
  deriving Show

-- | Convert an Incentivized Testnet (ITN) non-extended (Ed25519) signing or
-- verification key to a corresponding Shelley stake key
data KeyConvertITNKeyCmdArgs = KeyConvertITNKeyCmdArgs
  { itnKeyFile :: !(SomeKeyFile In)
  -- ^ Filepath of the ITN key (signing or verification)
  , outFile :: !(File () Out)
  -- ^ The output file
  }
  deriving Show

-- | Convert an Incentivized Testnet (ITN) extended (Ed25519Extended) signing key
-- to a corresponding Shelley stake signing key
data KeyConvertITNExtendedKeyCmdArgs = KeyConvertITNExtendedKeyCmdArgs
  { itnPrivKeyFile :: !(SomeKeyFile In)
  -- ^ Filepath of the ITN signing key
  , outFile :: !(File () Out)
  -- ^ The output file
  }
  deriving Show

-- | Convert an Incentivized Testnet (ITN) BIP32 (Ed25519Bip32) signing key to a
-- corresponding Shelley stake signing key
data KeyConvertITNBip32KeyCmdArgs = KeyConvertITNBip32KeyCmdArgs
  { itnPrivKeyFile :: !(SomeKeyFile In)
  -- ^ Filepath of the ITN signing key
  , outFile :: !(File () Out)
  -- ^ The output file
  }
  deriving Show

-- | Convert a cardano-address extended signing key to a corresponding
-- Shelley-format key
data KeyConvertCardanoAddressKeyCmdArgs = KeyConvertCardanoAddressKeyCmdArgs
  { cardanoAddressKeyType :: !CardanoAddressKeyType
  -- ^ Address key type of th signing key input file
  , skeyFileIn :: !(SigningKeyFile In)
  -- ^ Input filepath of the signing key
  , skeyFileOut :: !(File () Out)
  -- ^ The output file
  }
  deriving Show

renderKeyCmds :: KeyCmds -> Text
renderKeyCmds = \case
  KeyVerificationKeyCmd{} ->
    "key verification-key"
  KeyNonExtendedKeyCmd{} ->
    "key non-extended-key"
  KeyGenerateMnemonicCmd{} ->
    "key generate-mnemonic"
  KeyExtendedSigningKeyFromMnemonicCmd{} ->
    "key from-mnemonic"
  KeyConvertByronKeyCmd{} ->
    "key convert-byron-key"
  KeyConvertByronGenesisVKeyCmd{} ->
    "key convert-byron-genesis-vkey"
  KeyConvertITNKeyCmd{} ->
    "key convert-itn-key"
  KeyConvertITNExtendedKeyCmd{} ->
    "key convert-itn-extended-key"
  KeyConvertITNBip32KeyCmd{} ->
    "key convert-itn-bip32-key"
  KeyConvertCardanoAddressKeyCmd{} ->
    "key convert-cardano-address-key"
