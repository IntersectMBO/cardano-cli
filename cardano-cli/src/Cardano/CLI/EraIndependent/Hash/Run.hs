{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraIndependent.Hash.Run
  ( runHashCmds
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.EraIndependent.Hash.Command qualified as Cmd
import Cardano.CLI.EraIndependent.Hash.Internal.Common
import Cardano.CLI.Read
import Cardano.CLI.Type.Common
  ( GenesisFile (..)
  )
import Cardano.CLI.Type.Error.HashCmdError
import Cardano.Crypto.Hash (hashToTextAsHex)
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Prelude (ByteString)

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Function
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text

runHashCmds
  :: ()
  => Cmd.HashCmds
  -> ExceptT HashCmdError IO ()
runHashCmds = \case
  Cmd.HashAnchorDataCmd args -> runHashAnchorDataCmd args
  Cmd.HashScriptCmd args -> runHashScriptCmd args
  Cmd.HashGenesisFile args -> runHashGenesisFile args

runHashAnchorDataCmd
  :: ()
  => Cmd.HashAnchorDataCmdArgs
  -> ExceptT HashCmdError IO ()
runHashAnchorDataCmd Cmd.HashAnchorDataCmdArgs{toHash, hashGoal} = do
  anchorData <-
    L.AnchorData <$> case toHash of
      Cmd.AnchorDataHashSourceBinaryFile fp -> do
        let path = unFile fp
        handleIOExceptT (HashReadFileError path) $ BS.readFile path
      Cmd.AnchorDataHashSourceTextFile fp -> do
        let path = unFile fp
        text <- handleIOExceptT (HashReadFileError path) $ Text.readFile path
        return $ Text.encodeUtf8 text
      Cmd.AnchorDataHashSourceText text -> return $ Text.encodeUtf8 text
      Cmd.AnchorDataHashSourceURL urlText ->
        fetchURLToHashCmdError $ getByteStringFromURL allSchemes $ L.urlToText urlText
  let hash = L.hashAnchorData anchorData
  case hashGoal of
    Cmd.CheckHash expectedHash
      | hash /= expectedHash ->
          left $ HashMismatchedHashError expectedHash hash
      | otherwise -> do
          liftIO $ putStrLn "Hashes match!"
    Cmd.HashToFile outFile -> writeHash (Just outFile) hash
    Cmd.HashToStdout -> writeHash Nothing hash
 where
  writeHash :: Maybe (File () Out) -> L.SafeHash L.StandardCrypto i -> ExceptT HashCmdError IO ()
  writeHash mOutFile hash = do
    firstExceptT HashWriteFileError $
      newExceptT $
        writeTextOutput mOutFile text
   where
    text = hashToTextAsHex . L.extractHash $ hash

  fetchURLToHashCmdError
    :: ExceptT FetchURLError IO BS8.ByteString -> ExceptT HashCmdError IO BS8.ByteString
  fetchURLToHashCmdError = withExceptT HashFetchURLError

runHashScriptCmd
  :: ()
  => Cmd.HashScriptCmdArgs
  -> ExceptT HashCmdError IO ()
runHashScriptCmd Cmd.HashScriptCmdArgs{Cmd.toHash = File toHash, mOutFile} = do
  ScriptInAnyLang _ script <-
    readFileScriptInAnyLang toHash
      & firstExceptT (HashReadScriptError toHash)
  firstExceptT HashWriteFileError
    . newExceptT
    . writeTextOutput mOutFile
    . serialiseToRawBytesHexText
    $ hashScript script

runHashGenesisFile :: GenesisFile -> ExceptT HashCmdError IO ()
runHashGenesisFile (GenesisFile fpath) = do
  content <-
    handleIOExceptT (HashGenesisCmdGenesisFileError . FileIOError fpath) $
      BS.readFile fpath
  let gh :: Crypto.Hash Crypto.Blake2b_256 ByteString
      gh = Crypto.hashWith id content
  liftIO $ Text.putStrLn (Crypto.hashToTextAsHex gh)
