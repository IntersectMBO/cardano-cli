{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Run.Key
  ( runLegacyKeyCmds
  ) where

import           Cardano.Api

import qualified Cardano.CLI.EraBased.Commands.Key as Cmd
import           Cardano.CLI.EraBased.Run.Key
import           Cardano.CLI.Legacy.Commands.Key
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.KeyCmdError

import           Control.Monad.Trans.Except (ExceptT)
import           Data.Text (Text)

runLegacyKeyCmds :: ()
  => LegacyKeyCmds
  -> ExceptT KeyCmdError IO ()
runLegacyKeyCmds = \case
  KeyVerificationKeyCmd skf vkf ->
    runLegacyVerificationKeyCmd skf vkf
  KeyNonExtendedKeyCmd evkf vkf ->
    runLegacyNonExtendedKeyCmd evkf vkf
  KeyConvertByronKeyCmd mPassword keytype skfOld skfNew ->
    runLegacyConvertByronKeyCmd mPassword keytype skfOld skfNew
  KeyConvertByronGenesisKeyCmd oldVk newVkf ->
    runLegacyConvertByronGenesisKeyCmd oldVk newVkf
  KeyConvertITNKeyCmd itnKeyFile outFile ->
    runLegacyConvertITNStakeKeyCmd itnKeyFile outFile
  KeyConvertITNExtendedKeyCmd itnPrivKeyFile outFile ->
    runLegacyConvertITNExtendedKeyCmd itnPrivKeyFile outFile
  KeyConvertITNBip32KeyCmd itnPrivKeyFile outFile ->
    runLegacyConvertITNBip32KeyCmd itnPrivKeyFile outFile
  KeyConvertCardanoAddressKeyCmd keyType skfOld skfNew ->
    runLegacyConvertCardanoAddressKeyCmd keyType skfOld skfNew

runLegacyVerificationKeyCmd :: ()
  => SigningKeyFile In
  -> VerificationKeyFile Out
  -> ExceptT KeyCmdError IO ()
runLegacyVerificationKeyCmd skf vkf =
  runVerificationKeyCmd $
    Cmd.KeyVerificationKeyCmdArgs skf vkf

runLegacyNonExtendedKeyCmd :: ()
  => VerificationKeyFile In
  -> VerificationKeyFile Out
  -> ExceptT KeyCmdError IO ()
runLegacyNonExtendedKeyCmd evkf vkf =
  runNonExtendedKeyCmd $
    Cmd.KeyNonExtendedKeyCmdArgs evkf vkf

runLegacyConvertByronKeyCmd :: ()
  => Maybe Text      -- ^ Password (if applicable)
  -> ByronKeyType
  -> SomeKeyFile In  -- ^ Input file: old format
  -> File () Out     -- ^ Output file: new format
  -> ExceptT KeyCmdError IO ()
runLegacyConvertByronKeyCmd mPassword keytype skfOld skfNew =
  runConvertByronKeyCmd $
    Cmd.KeyConvertByronKeyCmdArgs mPassword keytype skfOld skfNew

runLegacyConvertByronGenesisKeyCmd :: ()
  => VerificationKeyBase64  -- ^ Input key raw old format
  -> File () Out            -- ^ Output file: new format
  -> ExceptT KeyCmdError IO ()
runLegacyConvertByronGenesisKeyCmd oldVk newVkf =
  runConvertByronGenesisKeyCmd $
    Cmd.KeyConvertByronGenesisKeyCmdArgs oldVk newVkf

--------------------------------------------------------------------------------
-- ITN verification/signing key conversion to Haskell verficiation/signing keys
--------------------------------------------------------------------------------

runLegacyConvertITNStakeKeyCmd :: ()
  => SomeKeyFile In
  -> File () Out
  -> ExceptT KeyCmdError IO ()
runLegacyConvertITNStakeKeyCmd itnKeyFile outFile =
  runConvertITNKeyCmd $
    Cmd.KeyConvertITNKeyCmdArgs itnKeyFile outFile

runLegacyConvertITNExtendedKeyCmd :: ()
  => SomeKeyFile In
  -> File () Out
  -> ExceptT KeyCmdError IO ()
runLegacyConvertITNExtendedKeyCmd itnPrivKeyFile outFile =
  runConvertITNExtendedKeyCmd $
    Cmd.KeyConvertITNExtendedKeyCmdArgs itnPrivKeyFile outFile

runLegacyConvertITNBip32KeyCmd :: ()
  => SomeKeyFile In
  -> File () Out
  -> ExceptT KeyCmdError IO ()
runLegacyConvertITNBip32KeyCmd itnPrivKeyFile outFile =
  runConvertITNBip32KeyCmd $
    Cmd.KeyConvertITNBip32KeyCmdArgs itnPrivKeyFile outFile

runLegacyConvertCardanoAddressKeyCmd :: ()
  => CardanoAddressKeyType
  -> SigningKeyFile In
  -> File () Out
  -> ExceptT KeyCmdError IO ()
runLegacyConvertCardanoAddressKeyCmd keyType skfOld skfNew =
  runConvertCardanoAddressKeyCmd $
    Cmd.KeyConvertCardanoAddressKeyCmdArgs keyType skfOld skfNew
