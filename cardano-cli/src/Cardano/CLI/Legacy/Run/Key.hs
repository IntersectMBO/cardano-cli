{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Run.Key
  ( runLegacyKeyCmds
  ) where

import           Cardano.Api

import qualified Cardano.CLI.EraBased.Run.Key as EraBased
import           Cardano.CLI.Legacy.Commands.Key
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.ShelleyKeyCmdError

import           Control.Monad.Trans.Except (ExceptT)
import           Data.Text (Text)

runLegacyKeyCmds :: LegacyKeyCmds -> ExceptT ShelleyKeyCmdError IO ()
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

runLegacyGetVerificationKeyCmd :: SigningKeyFile In
                      -> VerificationKeyFile Out
                      -> ExceptT ShelleyKeyCmdError IO ()
runLegacyGetVerificationKeyCmd = EraBased.runLegacyGetVerificationKeyCmd

runLegacyConvertToNonExtendedKeyCmd
  :: VerificationKeyFile In
  -> VerificationKeyFile Out
  -> ExceptT ShelleyKeyCmdError IO ()
runLegacyConvertToNonExtendedKeyCmd = EraBased.runLegacyConvertToNonExtendedKeyCmd

runLegacyConvertByronKeyCmd
  :: Maybe Text      -- ^ Password (if applicable)
  -> ByronKeyType
  -> SomeKeyFile In  -- ^ Input file: old format
  -> File () Out     -- ^ Output file: new format
  -> ExceptT ShelleyKeyCmdError IO ()
runLegacyConvertByronKeyCmd = EraBased.runLegacyConvertByronKeyCmd

runLegacyConvertByronGenesisVerificationKeyCmd
  :: VerificationKeyBase64  -- ^ Input key raw old format
  -> File () Out            -- ^ Output file: new format
  -> ExceptT ShelleyKeyCmdError IO ()
runLegacyConvertByronGenesisVerificationKeyCmd = EraBased.runLegacyConvertByronGenesisVerificationKeyCmd

--------------------------------------------------------------------------------
-- ITN verification/signing key conversion to Haskell verficiation/signing keys
--------------------------------------------------------------------------------

runLegacyConvertITNStakeKeyCmd
  :: SomeKeyFile In
  -> File () Out
  -> ExceptT ShelleyKeyCmdError IO ()
runLegacyConvertITNStakeKeyCmd = EraBased.runLegacyConvertITNStakeKeyCmd

runLegacyConvertITNExtendedToStakeKeyCmd :: SomeKeyFile In -> File () Out -> ExceptT ShelleyKeyCmdError IO ()
runLegacyConvertITNExtendedToStakeKeyCmd = EraBased.runLegacyConvertITNExtendedToStakeKeyCmd

runLegacyConvertITNBip32ToStakeKeyCmd :: SomeKeyFile In -> File () Out -> ExceptT ShelleyKeyCmdError IO ()
runLegacyConvertITNBip32ToStakeKeyCmd = EraBased.runLegacyConvertITNBip32ToStakeKeyCmd

runLegacyConvertCardanoAddressSigningKeyCmd
  :: CardanoAddressKeyType
  -> SigningKeyFile In
  -> File () Out
  -> ExceptT ShelleyKeyCmdError IO ()
runLegacyConvertCardanoAddressSigningKeyCmd = EraBased.runLegacyConvertCardanoAddressSigningKeyCmd
