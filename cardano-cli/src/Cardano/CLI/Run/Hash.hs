{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Run.Hash
  ( runHashCmds
  , getByteStringFromURL
  , SupportedSchemas (..)
  , allSchemas
  , httpsAndIpfsSchemas
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L

import           Cardano.CLI.Commands.Hash (HashGoal (..))
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
        fetchURLToHashCmdError $ getByteStringFromURL allSchemas urlText
  let hash = L.hashAnchorData anchorData
  case hashGoal of
    CheckHash expectedHash
      | hash /= expectedHash ->
          left $ HashMismatchedHashError expectedHash hash
      | otherwise -> do
          liftIO $ putStrLn "Hashes match!"
    HashToFile outFile -> writeHash (Just outFile) hash
    HashToStdout -> writeHash Nothing hash
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

data SupportedSchemas = FileSchema | HttpSchema | HttpsSchema | IpfsSchema
  deriving (Show, Eq)

allSchemas :: [SupportedSchemas]
allSchemas = [FileSchema, HttpSchema, HttpsSchema, IpfsSchema]

httpsAndIpfsSchemas :: [SupportedSchemas]
httpsAndIpfsSchemas = [HttpsSchema, IpfsSchema]

getByteStringFromURL :: [SupportedSchemas] -> L.Url -> ExceptT FetchURLError IO BS.ByteString
getByteStringFromURL supportedSchemas urlText = do
  let urlString = Text.unpack $ L.urlToText urlText
  uri <- hoistMaybe (FetchURLInvalidURLError urlString) $ parseAbsoluteURI urlString
  case map toLower $ uriScheme uri of
    "file:"
      | FileSchema `elem` supportedSchemas ->
          let path = uriPathToFilePath (pathSegments uri)
           in handleIOExceptT (FetchURLReadFileError path) $ BS.readFile path
    "http:" | HttpSchema `elem` supportedSchemas -> getFileFromHttp uri
    "https:" | HttpsSchema `elem` supportedSchemas -> getFileFromHttp uri
    "ipfs:" | IpfsSchema `elem` supportedSchemas -> do
      httpUri <- convertToHttp uri
      getFileFromHttp httpUri
    unsupportedScheme -> left $ FetchURLUnsupportedURLSchemeError unsupportedScheme
 where
  uriPathToFilePath :: [String] -> FilePath
  uriPathToFilePath allPath@(letter : path) =
    if isDrive letter
      then foldl (</>) letter path
      else foldl (</>) "/" allPath
  uriPathToFilePath [] = "/"

  getFileFromHttp :: URI -> ExceptT FetchURLError IO BS.ByteString
  getFileFromHttp uri = handlesExceptT handlers $ liftIO $ do
    request <- requestFromURI uri
    manager <- newManager tlsManagerSettings
    response <- httpLbs request manager
    let status = responseStatus response
    if statusCode status /= 200
      then throw $ BadStatusCodeHRE (statusCode status) (BS8.unpack $ statusMessage status)
      else return $ BS.concat . BSL.toChunks $ responseBody response

  handlers :: [Handler IO FetchURLError]
  handlers =
    [ mkHandler id
    , mkHandler HttpExceptionHRE
    , mkHandler IOExceptionHRE
    ]
   where
    mkHandler :: (Monad m, Exception e) => (e -> HttpRequestError) -> Handler m FetchURLError
    mkHandler x = Handler $ return . FetchURLGetFileFromHttpError . x

  convertToHttp :: URI -> ExceptT FetchURLError IO URI
  convertToHttp ipfsUri = do
    mIpfsGatewayUriString <- handleIOExceptT FetchURLReadEnvVarError $ IO.lookupEnv "IPFS_GATEWAY_URI"
    ipfsGatewayUriString <- hoistMaybe FetchURLIpfsGatewayNotSetError mIpfsGatewayUriString
    ipfsGatewayUri <-
      hoistMaybe (FetchURLInvalidURLError ipfsGatewayUriString) $ parseAbsoluteURI ipfsGatewayUriString
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
