{-# LANGUAGE FlexibleContexts #-}

module Test.Cardano.CLI.New.Network
  ( serveFilesWhile
  )
where

import Cardano.Api (MonadIO)

import Control.Concurrent (forkOS)
import Control.Monad (void)
import Control.Monad.Catch (MonadMask, bracket)
import Data.ByteString.UTF8 qualified as BSU8
import Data.Foldable (find)
import Data.List (intercalate)
import Data.String (IsString (fromString))
import Data.Text (unpack)
import Data.Text qualified as T
import Network.HTTP.Types.Status (status200, status404)
import Network.HTTP.Types.URI (renderQuery)
import Network.Socket (close)
import Network.Wai
  ( Request (..)
  , Response
  , ResponseReceived
  , pathInfo
  , responseFile
  , responseLBS
  )
import Network.Wai.Handler.Warp (defaultSettings, openFreePort, runSettingsSocket)

import Hedgehog as H
import Hedgehog.Internal.Source (HasCallStack)

-- | Takes a relative url (as a list of segments), a file path, and an action, and it serves
-- the file in the url provided in a random free port that is passed as a parameter to the
-- action. After the action returns, it shuts down the server. It returns the result of the
-- action. It also ensures the server is shut down even if the action throws an exception.
serveFilesWhile
  :: HasCallStack
  => MonadIO m
  => MonadMask m
  => MonadTest m
  => [([String], FilePath)]
  -- ^ List of pairs of a relative URL where a file will be served
  -- and the file path for the file to serve.
  -- Each element in the relative URL is a segment of the URL.
  -> (Int -> m a)
  -- ^ Action to run while the file is being served.
  -- It receives the port the server is listening on
  -> m a
serveFilesWhile relativeUrls action =
  bracket
    -- Server setup (resource acquisition)
    ( do
        -- Get the port the server is listening on
        (port, socket) <- H.evalIO openFreePort
        -- Serve the file
        let app :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
            app req respond = do
              let path = T.unpack <$> pathInfo req
              case find ((== path) . fst) relativeUrls of
                Just (_, filePath) ->
                  respond $ responseFile status200 [("Content-Type", "text/plain")] filePath Nothing
                Nothing ->
                  respond $
                    responseLBS status404 [("Content-Type", "text/plain")] $
                      fromString ("404 - Url \"" ++ urlFromRequest req ++ "\" - Not Found")

        -- Run server asynchronously in a separate thread
        void $ H.evalIO $ forkOS $ runSettingsSocket defaultSettings socket app
        return (port, socket)
    )
    -- Server teardown (resource release)
    (\(_, socket) -> H.evalIO $ close socket)
    -- Test action
    (\(port, _) -> action port)
 where
  urlFromRequest :: Request -> String
  urlFromRequest req =
    "http://"
      ++ maybe "localhost" BSU8.toString (requestHeaderHost req)
      ++ "/"
      ++ intercalate "/" (unpack <$> pathInfo req)
      ++ BSU8.toString (renderQuery True (queryString req))
