{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraIndependent.Hash.Run
  ( runHashCmds
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraIndependent.Hash.Command qualified as Cmd
import Cardano.CLI.EraIndependent.Hash.Internal.Common
import Cardano.CLI.Read
import Cardano.CLI.Type.Common
  ( GenesisFile (..)
  )
import Cardano.CLI.Type.Error.HashCmdError
import Cardano.Crypto.Hash (hashToTextAsHex)
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Ledger.Hashes qualified as L

import RIO

import Data.ByteString.Char8 qualified as BS8
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text

runHashCmds
  :: ()
  => Cmd.HashCmds
  -> CIO e ()
runHashCmds = \case
  Cmd.HashAnchorDataCmd args -> runHashAnchorDataCmd args
  Cmd.HashScriptCmd args ->
    runHashScriptCmd args
  Cmd.HashGenesisFile args -> runHashGenesisFile args

runHashAnchorDataCmd
  :: ()
  => Cmd.HashAnchorDataCmdArgs
  -> CIO e ()
runHashAnchorDataCmd Cmd.HashAnchorDataCmdArgs{toHash, hashGoal} = do
  anchorData <-
    L.AnchorData <$> case toHash of
      Cmd.AnchorDataHashSourceBinaryFile fp -> do
        let path = unFile fp
        readFileCli path
      Cmd.AnchorDataHashSourceTextFile fp -> do
        let path = unFile fp
        text <- readFileCli path
        return text
      Cmd.AnchorDataHashSourceText text -> return $ Text.encodeUtf8 text
      Cmd.AnchorDataHashSourceURL urlText ->
        fromExceptTCli $
          fetchURLToHashCmdError $
            getByteStringFromURL allSchemes $
              L.urlToText urlText
  let hash = L.hashAnnotated anchorData
  case hashGoal of
    Cmd.CheckHash expectedHash
      | hash /= expectedHash ->
          throwCliError $ HashMismatchedHashError expectedHash hash
      | otherwise -> do
          liftIO $ putStrLn "Hashes match!"
    Cmd.HashToFile outFile -> writeHash (Just outFile) hash
    Cmd.HashToStdout -> writeHash Nothing hash
 where
  writeHash :: Maybe (File () Out) -> L.SafeHash i -> CIO e ()
  writeHash mOutFile hash = do
    fromEitherIOCli @(FileError ()) $
      writeTextOutput mOutFile text
   where
    text = hashToTextAsHex . L.extractHash $ hash

  fetchURLToHashCmdError
    :: ExceptT FetchURLError IO BS8.ByteString -> ExceptT HashCmdError IO BS8.ByteString
  fetchURLToHashCmdError = withExceptT HashFetchURLError

runHashScriptCmd
  :: ()
  => Cmd.HashScriptCmdArgs
  -> CIO e ()
runHashScriptCmd Cmd.HashScriptCmdArgs{Cmd.toHash = File toHash, mOutFile} = do
  ScriptInAnyLang _ script <-
    readFileScriptInAnyLang toHash
  fromEitherIOCli @(FileError ()) $
    writeTextOutput mOutFile $
      serialiseToRawBytesHexText $
        hashScript script

runHashGenesisFile :: GenesisFile -> CIO e ()
runHashGenesisFile (GenesisFile fpath) = do
  content <- readFileCli fpath
  let gh :: Crypto.Hash Crypto.Blake2b_256 ByteString
      gh = Crypto.hashWith id content
  liftIO $ Text.putStrLn (Crypto.hashToTextAsHex gh)
