{-# LANGUAGE FlexibleContexts #-}

module Test.Cli.Hash where

import           Cardano.Api (MonadIO)

import           Control.Concurrent (forkOS)
import           Control.Exception.Lifted (bracket)
import           Control.Monad (void)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.List (intercalate)
import qualified Data.Text as T
import           GHC.IO.Exception (ExitCode (ExitFailure))
import           Network.HTTP.Types.Status (status200, status404)
import           Network.Socket (close)
import           Network.Wai (Request, Response, ResponseReceived, pathInfo, responseFile,
                   responseLBS)
import           Network.Wai.Handler.Warp (defaultSettings, openFreePort, runSettingsSocket)
import           System.Directory (getCurrentDirectory)
import           System.FilePath (dropTrailingPathSeparator)
import           System.FilePath.Posix (splitDirectories)

import           Test.Cardano.CLI.Util

import           Hedgehog as H
import qualified Hedgehog.Extras as H
import           Hedgehog.Internal.Source (HasCallStack)

exampleAnchorDataHash :: String
exampleAnchorDataHash = "de38a4f5b8b9d8372386cc923bad19d1a0662298cf355bbe947e5eedf127fa9c"

exampleAnchorDataPath :: String
exampleAnchorDataPath = "test/cardano-cli-test/files/input/example_anchor_data.txt"

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/generate anchor data hash from file/"'@
hprop_generate_anchor_data_hash_from_file :: Property
hprop_generate_anchor_data_hash_from_file =
  propertyOnce $ do
    result <-
      execCardanoCLI
        [ "hash"
        , "anchor-data"
        , "--file-binary"
        , exampleAnchorDataPath
        ]
    result === exampleAnchorDataHash

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/check anchor data hash from file/"'@
hprop_check_anchor_data_hash_from_file :: Property
hprop_check_anchor_data_hash_from_file =
  propertyOnce $ do
    void $
      execCardanoCLI
        [ "hash"
        , "anchor-data"
        , "--file-binary"
        , exampleAnchorDataPath
        , "--expected-hash"
        , exampleAnchorDataHash
        ]

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/check anchor data hash from file fails/"'@
hprop_check_anchor_data_hash_from_file_fails :: Property
hprop_check_anchor_data_hash_from_file_fails =
  propertyOnce $ do
    (ec, _, _) <-
      execDetailCardanoCLI
        [ "hash"
        , "anchor-data"
        , "--file-binary"
        , exampleAnchorDataPath
        , "--expected-hash"
        , 'c' : drop 1 exampleAnchorDataHash
        ]
    ec === ExitFailure 1

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/generate anchor data hash from file uri/"'@
hprop_generate_anchor_data_hash_from_file_uri :: Property
hprop_generate_anchor_data_hash_from_file_uri =
  propertyOnce $ do
    cwd <- H.evalIO getCurrentDirectory
    posixCwd <- toPOSIX cwd
    result <-
      execCardanoCLI
        [ "hash"
        , "anchor-data"
        , "--url"
        , "file://" ++ posixCwd ++ "/" ++ exampleAnchorDataPath
        ]
    result === exampleAnchorDataHash
 where
  toPOSIX :: FilePath -> PropertyT IO [Char]
  toPOSIX path =
    case map dropTrailingPathSeparator (splitDirectories path) of
      letter : restOfPath -> do
        fixedLetter <- case letter of
          '/' : _ -> return ""
          l : ':' : _ -> return $ l : ":/"
          wrongLetter -> do
            H.note_ ("Unexpected letter: " ++ wrongLetter)
            H.failure
        return $ fixedLetter ++ intercalate "/" (fixedLetter : restOfPath)
      [] -> do
        H.note_ ("Path doesn't split:" ++ path)
        H.failure

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/check anchor data hash from http uri/"'@
hprop_check_anchor_data_hash_from_http_uri :: Property
hprop_check_anchor_data_hash_from_http_uri =
  propertyOnce $ do
    let relativeUrl = ["example", "url", "file.txt"]
    serveFileWhile
      relativeUrl
      exampleAnchorDataPath
      ( \port ->
          void $
            execCardanoCLI
              [ "hash"
              , "anchor-data"
              , "--url"
              , "http://localhost:" ++ show port ++ "/" ++ intercalate "/" relativeUrl
              , "--expected-hash"
              , exampleAnchorDataHash
              ]
      )

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
                -- Status -> ResponseHeaders -> FilePath -> Maybe FilePart -> Response
                then respond $ responseFile status200 [("Content-Type", "text/plain")] filePath Nothing
                else respond $ responseLBS status404 [("Content-Type", "text/plain")] "404 - Not Found"

        -- Run server asynchronously in a separate thread
        void $ H.evalIO $ forkOS $ runSettingsSocket defaultSettings socket app
        return (port, socket)
    )
    -- Server teardown (resource release)
    (\(_, socket) -> H.evalIO $ close socket)
    -- Test action
    (\(port, _) -> action port)
