{-# LANGUAGE FlexibleContexts #-}

module Test.Cardano.CLI.Hash
  ( exampleAnchorDataHash
  , serveFileWhile
  , exampleAnchorDataPathTest
  , exampleAnchorDataPathGolden
  , exampleAnchorDataIpfsHash
  )
where

import           Cardano.Api (MonadIO)

import           Control.Concurrent (forkOS)
import           Control.Exception.Lifted (bracket)
import           Control.Monad (void)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.ByteString.UTF8 as BSU8
import           Data.List (intercalate)
import           Data.String (IsString (fromString))
import           Data.Text (unpack)
import qualified Data.Text as T
import           Network.HTTP.Types.Status (status200, status404)
import           Network.HTTP.Types.URI (renderQuery)
import           Network.Socket (close)
import           Network.Wai (Request (..), Response, ResponseReceived, pathInfo, responseFile,
                   responseLBS)
import           Network.Wai.Handler.Warp (defaultSettings, openFreePort, runSettingsSocket)

import           Hedgehog as H
import           Hedgehog.Internal.Source (HasCallStack)

exampleAnchorDataHash :: String
exampleAnchorDataHash = "de38a4f5b8b9d8372386cc923bad19d1a0662298cf355bbe947e5eedf127fa9c"

exampleAnchorDataPathGolden :: String
exampleAnchorDataPathGolden = "test/cardano-cli-golden/files/input/example_anchor_data.txt"

exampleAnchorDataPathTest :: String
exampleAnchorDataPathTest = "test/cardano-cli-test/files/input/example_anchor_data.txt"

exampleAnchorDataIpfsHash :: String
exampleAnchorDataIpfsHash = "QmbL5EBFJLf8DdPkWAskG3Euin9tHY8naqQ2JDoHnWHHXJ"

-- | Takes a relative url (as a list of segments), a file path, and an action, and it serves
-- the file in the url provided in a random free port that is passed as a parameter to the
-- action. After the action returns, it shuts down the server. It returns the result of the
-- action. It also ensures the server is shut down even if the action throws an exception.
serveFileWhile
  :: (MonadBaseControl IO m, MonadTest m, MonadIO m, HasCallStack)
  => [String]
  -- ^ Relative URL where the file will be served.
  -- Each element is a segment of the URL.
  -> FilePath
  -- ^ File path for the file to serve
  -> (Int -> m a)
  -- ^ Action to run while the file is being served.
  -- It receives the port the server is listening on
  -> m a
serveFileWhile relativeUrl filePath action =
  bracket
    -- Server setup (resource acquisition)
    ( do
        -- Get the port the server is listening on
        (port, socket) <- H.evalIO openFreePort
        -- Serve the file
        let app :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
            app req respond = do
              let path = T.unpack <$> pathInfo req
              if path == relativeUrl
                then respond $ responseFile status200 [("Content-Type", "text/plain")] filePath Nothing
                else
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
