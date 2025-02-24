{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraIndependent.Hash.Internal.Common
  ( getByteStringFromURL
  , carryHashChecks
  , allSchemes
  , httpsAndIpfsSchemes
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Parser (stringToAnchorScheme)
import Cardano.CLI.Type.Common
  ( AnchorScheme (..)
  , MustCheckHash (..)
  , PotentiallyCheckedAnchor (..)
  , SupportedSchemes
  )
import Cardano.CLI.Type.Error.HashCmdError
import Cardano.Prelude (first)

import Control.Exception (throw)
import Control.Monad (when)
import Control.Monad.Catch (Exception, Handler (Handler))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Client (Response (..), httpLbs, newManager, requestFromURI)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Status (statusCode), statusMessage)
import Network.URI (URI (..), URIAuth (..), parseAbsoluteURI, pathSegments)
import System.Environment qualified as IO
import System.FilePath ((</>))
import System.FilePath.Posix (isDrive)

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

-- | Check the hash of the anchor data against the hash in the anchor if
-- checkHash is set to CheckHash.
carryHashChecks
  :: PotentiallyCheckedAnchor anchorType (L.Anchor L.StandardCrypto)
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
      let hash = L.hashAnchorData anchorData
      when (hash /= L.anchorDataHash anchor) $
        left $
          HashMismatchError (L.anchorDataHash anchor) hash
    TrustHash -> pure ()
 where
  anchor = pcaAnchor potentiallyCheckedAnchor

-- | All the supported schemes are allowed.
allSchemes :: SupportedSchemes
allSchemes = [FileScheme, HttpScheme, HttpsScheme, IpfsScheme]
