
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use let" -}

module Cardano.CLI.EraBased.Run.Governance.Hash
  ( runGovernanceHashCmds
  ) where

import           Cardano.Api

import qualified Cardano.CLI.EraBased.Commands.Governance.Hash as Cmd
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.CmdError
import           Cardano.CLI.Types.Errors.GovernanceCmdError
import           Cardano.CLI.Types.Errors.GovernanceHashError
import           Cardano.Crypto.Hash (hashToTextAsHex)
import qualified Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Crypto
import           Cardano.Ledger.SafeHash (extractHash)
import qualified Cardano.Ledger.SafeHash as Ledger

import qualified Data.ByteString as BS
import           Data.Function
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text

runGovernanceHashCmds :: ()
  => Cmd.GovernanceHashCmds era
  -> ExceptT CmdError IO ()
runGovernanceHashCmds = \case

  Cmd.GovernanceHashAnchorDataCmd args ->
    runGovernanceHashAnchorDataCmd args
      & firstExceptT (CmdGovernanceCmdError . GovernanceCmdHashError)

  Cmd.GovernanceHashScriptCmd args ->
    runGovernanceHashScriptCmd args
      & firstExceptT (CmdGovernanceCmdError . GovernanceCmdHashError)

runGovernanceHashAnchorDataCmd :: ()
  => Cmd.GovernanceHashAnchorDataCmdArgs era
  -> ExceptT GovernanceHashError IO ()
runGovernanceHashAnchorDataCmd Cmd.GovernanceHashAnchorDataCmdArgs { toHash, moutFile } =
  case toHash of
    Cmd.GovernanceAnchorDataHashSourceBinaryFile fp -> do
      let path = unFile fp
      bytes <- handleIOExceptT (GovernanceHashReadFileError path) $ BS.readFile path
      let hash = Ledger.hashAnchorData $ Ledger.AnchorData bytes
      printHash hash
    Cmd.GovernanceAnchorDataHashSourceTextFile fp -> do
      let path = unFile fp
      text <- handleIOExceptT (GovernanceHashReadFileError path) $ Text.readFile path
      let hash = Ledger.hashAnchorData $ Ledger.AnchorData $ Text.encodeUtf8 text
      printHash hash
    Cmd.GovernanceAnchorDataHashSourceText text -> do
      let hash = Ledger.hashAnchorData $ Ledger.AnchorData $ Text.encodeUtf8 text
      printHash hash
  where
    printHash :: Ledger.SafeHash StandardCrypto i -> ExceptT GovernanceHashError IO ()
    printHash hash = do
      firstExceptT GovernanceHashWriteFileError $
        newExceptT $ writeTextOutput moutFile text
      where
        text = hashToTextAsHex . extractHash $ hash

runGovernanceHashScriptCmd :: ()
  => Cmd.GovernanceHashScriptCmdArgs era
  -> ExceptT GovernanceHashError IO ()
runGovernanceHashScriptCmd Cmd.GovernanceHashScriptCmdArgs { Cmd.toHash = ScriptFile toHash, moutFile } = do
  ScriptInAnyLang _ script <-
    readFileScriptInAnyLang toHash
      & firstExceptT (GovernanceHashReadScriptError toHash)
  firstExceptT GovernanceHashWriteFileError
    . newExceptT
    . writeTextOutput moutFile . serialiseToRawBytesHexText $ hashScript script


