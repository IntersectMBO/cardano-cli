{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Run.Key
  ( runLegacyKeyCmds
  ) where

import Cardano.Api

import Cardano.CLI.EraBased.Run.Key
import Cardano.CLI.Legacy.Commands.Key
import Cardano.CLI.Types.Common
import Cardano.CLI.Types.Errors.KeyCmdError

import Control.Monad.Trans.Except (ExceptT)
import Data.Text (Text)

runLegacyKeyCmds
  :: ()
  => LegacyKeyCmds
  -> ExceptT KeyCmdError IO ()
runLegacyKeyCmds = \case
  KeyGetVerificationKey skf vkf ->
    runLegacyGetVerificationKeyCmd skf vkf
  KeyNonExtendedKey evkf vkf ->
    runLegacyConvertToNonExtendedKeyCmd evkf vkf
  KeyConvertByronKey mPassword keytype skfOld skfNew ->
    runLegacyConvertByronKeyCmd mPassword keytype skfOld skfNew
  KeyConvertByronGenesisVKey oldVk newVkf ->
    runLegacyConvertByronGenesisVerificationKeyCmd oldVk newVkf
  KeyConvertITNStakeKey itnKeyFile outFile ->
    runLegacyConvertITNStakeKeyCmd itnKeyFile outFile
  KeyConvertITNExtendedToStakeKey itnPrivKeyFile outFile ->
    runLegacyConvertITNExtendedToStakeKeyCmd itnPrivKeyFile outFile
  KeyConvertITNBip32ToStakeKey itnPrivKeyFile outFile ->
    runLegacyConvertITNBip32ToStakeKeyCmd itnPrivKeyFile outFile
  KeyConvertCardanoAddressSigningKey keyType skfOld skfNew ->
    runLegacyConvertCardanoAddressSigningKeyCmd keyType skfOld skfNew

runLegacyGetVerificationKeyCmd
  :: ()
  => SigningKeyFile In
  -> VerificationKeyFile Out
  -> ExceptT KeyCmdError IO ()
runLegacyGetVerificationKeyCmd = runGetVerificationKeyCmd

runLegacyConvertToNonExtendedKeyCmd
  :: ()
  => VerificationKeyFile In
  -> VerificationKeyFile Out
  -> ExceptT KeyCmdError IO ()
runLegacyConvertToNonExtendedKeyCmd = runConvertToNonExtendedKeyCmd

runLegacyConvertByronKeyCmd
  :: ()
  => Maybe Text
  -- ^ Password (if applicable)
  -> ByronKeyType
  -> SomeKeyFile In
  -- ^ Input file: old format
  -> File () Out
  -- ^ Output file: new format
  -> ExceptT KeyCmdError IO ()
runLegacyConvertByronKeyCmd = runConvertByronKeyCmd

runLegacyConvertByronGenesisVerificationKeyCmd
  :: ()
  => VerificationKeyBase64
  -- ^ Input key raw old format
  -> File () Out
  -- ^ Output file: new format
  -> ExceptT KeyCmdError IO ()
runLegacyConvertByronGenesisVerificationKeyCmd = runConvertByronGenesisVerificationKeyCmd

--------------------------------------------------------------------------------
-- ITN verification/signing key conversion to Haskell verficiation/signing keys
--------------------------------------------------------------------------------

runLegacyConvertITNStakeKeyCmd
  :: ()
  => SomeKeyFile In
  -> File () Out
  -> ExceptT KeyCmdError IO ()
runLegacyConvertITNStakeKeyCmd = runConvertITNStakeKeyCmd

runLegacyConvertITNExtendedToStakeKeyCmd
  :: ()
  => SomeKeyFile In
  -> File () Out
  -> ExceptT KeyCmdError IO ()
runLegacyConvertITNExtendedToStakeKeyCmd = runConvertITNExtendedToStakeKeyCmd

runLegacyConvertITNBip32ToStakeKeyCmd
  :: ()
  => SomeKeyFile In
  -> File () Out
  -> ExceptT KeyCmdError IO ()
runLegacyConvertITNBip32ToStakeKeyCmd = runConvertITNBip32ToStakeKeyCmd

runLegacyConvertCardanoAddressSigningKeyCmd
  :: ()
  => CardanoAddressKeyType
  -> SigningKeyFile In
  -> File () Out
  -> ExceptT KeyCmdError IO ()
runLegacyConvertCardanoAddressSigningKeyCmd = runConvertCardanoAddressSigningKeyCmd
