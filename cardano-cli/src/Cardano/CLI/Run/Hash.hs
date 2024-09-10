{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Run.Hash
  ( runHashCmds
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L

import qualified Cardano.CLI.Commands.Hash as Cmd
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Errors.HashCmdError
import           Cardano.Crypto.Hash (hashToTextAsHex)

import           Control.Exception (throw)
import           Control.Monad.Catch (Exception, Handler (Handler))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import           Data.Char (toLower)
import           Data.Function
import           Data.List (intercalate)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import           Network.HTTP.Client (Response (..), httpLbs, newManager, requestFromURI)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types (Status (statusCode), statusMessage)
import           Network.URI (URI (..), URIAuth (..), parseAbsoluteURI, pathSegments)
import qualified System.Environment as IO
import           System.FilePath ((</>))
import           System.FilePath.Posix (isDrive)

runHashCmds
  :: ()
  => Cmd.HashCmds
  -> ExceptT HashCmdError IO ()
runHashCmds = \case
  Cmd.HashAnchorDataCmd args -> runHashAnchorDataCmd args
  Cmd.HashScriptCmd args -> runHashScriptCmd args

runHashAnchorDataCmd
  :: ()
  => Cmd.HashAnchorDataCmdArgs
  -> ExceptT HashCmdError IO ()
runHashAnchorDataCmd Cmd.HashAnchorDataCmdArgs{toHash, mExpectedHash, mOutFile} = do
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
        getByteStringFromURL urlText
  let hash = L.hashAnchorData anchorData
  case mExpectedHash of
    Just expectedHash
      | hash /= expectedHash ->
          left $ HashMismatchedHashError expectedHash hash
      | otherwise -> liftIO $ putStrLn "Hashes match!"
    Nothing -> writeHash hash
 where
  writeHash :: L.SafeHash L.StandardCrypto i -> ExceptT HashCmdError IO ()
  writeHash hash = do
    firstExceptT HashWriteFileError $
      newExceptT $
        writeTextOutput mOutFile text
   where
    text = hashToTextAsHex . L.extractHash $ hash

  getByteStringFromURL :: L.Url -> ExceptT HashCmdError IO BS.ByteString
  getByteStringFromURL urlText = do
    let urlString = Text.unpack $ L.urlToText urlText
    uri <- expectJustExceptT (HashInvalidURLError urlString) $ parseAbsoluteURI urlString
    case map toLower $ uriScheme uri of
      "file:" ->
        let path = uriPathToFilePath (pathSegments uri)
         in handleIOExceptT (HashReadFileError path) $ BS.readFile path
      "http:" -> getFileFromHttp uri
      "https:" -> getFileFromHttp uri
      "ipfs:" -> do
        httpUri <- convertToHttp uri
        getFileFromHttp httpUri
      unsupportedScheme -> left $ HashUnsupportedURLSchemeError unsupportedScheme
   where
    uriPathToFilePath :: [String] -> FilePath
    uriPathToFilePath allPath@(letter : path) =
      if isDrive letter
        then foldl (</>) letter path
        else foldl (</>) "/" allPath
    uriPathToFilePath [] = "/"

    getFileFromHttp :: URI -> ExceptT HashCmdError IO BS.ByteString
    getFileFromHttp uri = handlesExceptT handlers $ liftIO $ do
      request <- requestFromURI uri
      manager <- newManager tlsManagerSettings
      response <- httpLbs request manager
      let status = responseStatus response
      if statusCode status /= 200
        then throw $ BadStatusCodeHRE (statusCode status) (BS8.unpack $ statusMessage status)
        else return $ BS.concat . BSL.toChunks $ responseBody response
     where
      handlers :: [Handler IO HashCmdError]
      handlers =
        [ mkHandler id
        , mkHandler HttpExceptionHRE
        , mkHandler IOExceptionHRE
        ]
       where
        mkHandler :: (Monad m, Exception e) => (e -> HttpRequestError) -> Handler m HashCmdError
        mkHandler x = Handler $ return . HashGetFileFromHttpError . x

    convertToHttp :: URI -> ExceptT HashCmdError IO URI
    convertToHttp ipfsUri = do
      mIpfsGatewayUriString <- handleIOExceptT HashReadEnvVarError $ IO.lookupEnv "IPFS_GATEWAY_URI"
      ipfsGatewayUriString <- expectJustExceptT HashIpfsGatewayNotSetError mIpfsGatewayUriString
      ipfsGatewayUri <-
        expectJustExceptT (HashInvalidURLError ipfsGatewayUriString) $ parseAbsoluteURI ipfsGatewayUriString
      return $
        ipfsGatewayUri
          { uriPath =
              '/'
                : intercalate
                  "/"
                  ( pathSegments ipfsGatewayUri
                      ++ ["ipfs"]
                      ++ maybe [] (\ipfsAuthority -> [uriRegName ipfsAuthority]) (uriAuthority ipfsUri)
                      ++ pathSegments ipfsUri
                  )
          }

  expectJustExceptT :: e -> Maybe a -> ExceptT e IO a
  expectJustExceptT e Nothing = left e
  expectJustExceptT _ (Just a) = return a

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
