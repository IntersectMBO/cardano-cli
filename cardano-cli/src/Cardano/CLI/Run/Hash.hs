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
  , carryHashChecks
  , allSchemes
  , httpsAndIpfsSchemes
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Commands.Hash qualified as Cmd
import Cardano.CLI.Parser (stringToAnchorScheme)
import Cardano.CLI.Read
import Cardano.CLI.Types.Common
  ( AnchorScheme (..)
  , GenesisFile (..)
  , MustCheckHash (..)
  , PotentiallyCheckedAnchor (..)
  , SupportedSchemes
  )
import Cardano.CLI.Types.Errors.HashCmdError
import Cardano.Crypto.Hash (hashToTextAsHex)
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Ledger.Hashes qualified as L
import Cardano.Prelude (ByteString, first)

import Control.Exception (throw)
import Control.Monad (when)
import Control.Monad.Catch (Exception, Handler (Handler))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.Function
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Network.HTTP.Client (Response (..), httpLbs, newManager, requestFromURI)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Status (statusCode), statusMessage)
import Network.URI (URI (..), URIAuth (..), parseAbsoluteURI, pathSegments)
import System.Environment qualified as IO
import System.FilePath ((</>))
import System.FilePath.Posix (isDrive)

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
  let hash = L.hashAnnotated anchorData
  case hashGoal of
    Cmd.CheckHash expectedHash
      | hash /= expectedHash ->
          left $ HashMismatchedHashError expectedHash hash
      | otherwise -> do
          liftIO $ putStrLn "Hashes match!"
    Cmd.HashToFile outFile -> writeHash (Just outFile) hash
    Cmd.HashToStdout -> writeHash Nothing hash
 where
  writeHash :: Maybe (File () Out) -> L.SafeHash i -> ExceptT HashCmdError IO ()
  writeHash mOutFile hash = do
    firstExceptT HashWriteFileError $
      newExceptT $
        writeTextOutput mOutFile text
   where
    text = hashToTextAsHex . L.extractHash $ hash

  fetchURLToHashCmdError
    :: ExceptT FetchURLError IO BS8.ByteString -> ExceptT HashCmdError IO BS8.ByteString
  fetchURLToHashCmdError = withExceptT HashFetchURLError

-- | All the supported schemes are allowed.
allSchemes :: SupportedSchemes
allSchemes = [FileScheme, HttpScheme, HttpsScheme, IpfsScheme]

-- | Only HTTPS and IPFS schemes are allowed. We also allow HTTP for testing purposes
-- but it is discouraged, because it can lead to security vulnerabilities.
-- For example: If a user checks the anchor-data through a web browser and through the
-- `cardano-cli` independently, one of them could easily get spoofed, and the user would
-- not notice that the anchor-data being verified in the browser is not the same.
httpsAndIpfsSchemes :: SupportedSchemes
httpsAndIpfsSchemes =
  [ HttpScheme -- Insecure, only for testing purposes
  , HttpsScheme
  , IpfsScheme
  ]

-- | Fetches the content of a URL as a 'ByteString'.
-- The URL must be an absolute URL. The supported schemes are specified in the 'SupportedSchemes' argument.
-- If the scheme is not supported, an error is thrown.
getByteStringFromURL :: SupportedSchemes -> Text -> ExceptT FetchURLError IO BS.ByteString
getByteStringFromURL supportedSchemes urlText = do
  let urlString = Text.unpack urlText
  uri@URI{uriScheme} <- hoistMaybe (FetchURLInvalidURLError urlString) $ parseAbsoluteURI urlString
  scheme <-
    hoistEither $
      first FetchURLUnsupportedURLSchemeError $
        stringToAnchorScheme supportedSchemes uriScheme
  case scheme of
    FileScheme ->
      let path = uriPathToFilePath (pathSegments uri)
       in handleIOExceptT (FetchURLReadFileError path) $ BS.readFile path
    HttpScheme -> getFileFromHttp uri
    HttpsScheme -> getFileFromHttp uri
    IpfsScheme -> do
      httpUri <- convertToHttp uri
      getFileFromHttp httpUri
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
      then
        throw $
          BadStatusCodeHRE
            (statusCode status)
            (BS8.unpack (statusMessage status) ++ ": " ++ BSL8.unpack (responseBody response))
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

-- | Check the hash of the anchor data against the hash in the anchor if
-- checkHash is set to CheckHash.
carryHashChecks
  :: PotentiallyCheckedAnchor anchorType L.Anchor
  -- ^ The information about anchor data and whether to check the hash (see 'PotentiallyCheckedAnchor')
  -> ExceptT HashCheckError IO ()
carryHashChecks potentiallyCheckedAnchor =
  case pcaMustCheck potentiallyCheckedAnchor of
    CheckHash -> do
      anchorData <-
        L.AnchorData
          <$> withExceptT
            FetchURLError
            (getByteStringFromURL httpsAndIpfsSchemes $ L.urlToText $ L.anchorUrl anchor)
      let hash = L.hashAnnotated anchorData
      when (hash /= L.anchorDataHash anchor) $
        left $
          HashMismatchError (L.anchorDataHash anchor) hash
    TrustHash -> pure ()
 where
  anchor = pcaAnchor potentiallyCheckedAnchor

runHashGenesisFile :: GenesisFile -> ExceptT HashCmdError IO ()
runHashGenesisFile (GenesisFile fpath) = do
  content <-
    handleIOExceptT (HashGenesisCmdGenesisFileError . FileIOError fpath) $
      BS.readFile fpath
  let gh :: Crypto.Hash Crypto.Blake2b_256 ByteString
      gh = Crypto.hashWith id content
  liftIO $ Text.putStrLn (Crypto.hashToTextAsHex gh)
