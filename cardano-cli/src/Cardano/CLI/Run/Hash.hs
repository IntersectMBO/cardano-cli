
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Run.Hash
  ( runHashCmds
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L

import qualified Cardano.CLI.Commands.Hash as Cmd
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Errors.HashCmdError
import           Cardano.Crypto.Hash (hashToTextAsHex)

import qualified Data.ByteString as BS
import           Data.Function
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text

runHashCmds :: ()
  => Cmd.HashCmds
  -> ExceptT HashCmdError IO ()
runHashCmds = \case
  Cmd.HashAnchorDataCmd args -> runHashAnchorDataCmd args
  Cmd.HashScriptCmd     args -> runHashScriptCmd     args

runHashAnchorDataCmd :: ()
  => Cmd.HashAnchorDataCmdArgs
  -> ExceptT HashCmdError IO ()
runHashAnchorDataCmd Cmd.HashAnchorDataCmdArgs { toHash, mOutFile } =
  case toHash of
    Cmd.AnchorDataHashSourceBinaryFile fp -> do
      let path = unFile fp
      bytes <- handleIOExceptT (HashReadFileError path) $ BS.readFile path
      let hash = L.hashAnchorData $ L.AnchorData bytes
      writeHash hash
    Cmd.AnchorDataHashSourceTextFile fp -> do
      let path = unFile fp
      text <- handleIOExceptT (HashReadFileError path) $ Text.readFile path
      let hash = L.hashAnchorData $ L.AnchorData $ Text.encodeUtf8 text
      writeHash hash
    Cmd.AnchorDataHashSourceText text -> do
      let hash = L.hashAnchorData $ L.AnchorData $ Text.encodeUtf8 text
      writeHash hash
  where
    writeHash :: L.SafeHash L.StandardCrypto i -> ExceptT HashCmdError IO ()
    writeHash hash = do
      firstExceptT HashWriteFileError $
        newExceptT $ writeTextOutput mOutFile text
      where
        text = hashToTextAsHex . L.extractHash $ hash

runHashScriptCmd :: ()
  => Cmd.HashScriptCmdArgs
  -> ExceptT HashCmdError IO ()
runHashScriptCmd Cmd.HashScriptCmdArgs { Cmd.toHash = File toHash, mOutFile } = do
  ScriptInAnyLang _ script <-
    readFileScriptInAnyLang toHash
      & firstExceptT (HashReadScriptError toHash)
  firstExceptT HashWriteFileError
    . newExceptT
    . writeTextOutput mOutFile . serialiseToRawBytesHexText $ hashScript script

