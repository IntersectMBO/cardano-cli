{-# LANGUAGE FlexibleContexts #-}

module Test.Cardano.CLI.Hash
  ( exampleAnchorDataHash
  , exampleAnchorDataHash2
  , serveFilesWhile
  , exampleAnchorDataPathTest
  , exampleAnchorDataPathTest2
  , exampleAnchorDataPathGolden
  , exampleAnchorDataPathGolden2
  , exampleAnchorDataIpfsHash
  , exampleAnchorDataIpfsHash2
  , tamperBase16Hash
  , exampleDRepRegAnchorHash
  , exampleDRepRegAnchorIpfsHash
  , exampleDRepRegAnchorPathTest
  , exampleDRepRegAnchorPathGolden
  , exampleGovActionAnchorHash1
  , exampleGovActionAnchorHash2
  , exampleGovActionAnchorPathGolden1
  , exampleGovActionAnchorPathGolden2
  , exampleGovActionAnchorPathTest1
  , exampleGovActionAnchorPathTest2
  , exampleGovActionAnchorIpfsHash1
  , exampleGovActionAnchorIpfsHash2
  )
where

import           Cardano.Api (MonadIO)

import           Control.Concurrent (forkOS)
import           Control.Exception.Lifted (bracket)
import           Control.Monad (void)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.ByteString.UTF8 as BSU8
import           Data.Char (toLower)
import           Data.Foldable (find)
import           Data.List (elemIndex, intercalate)
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

-- ** Anchor hashes for the example files

exampleAnchorDataHash, exampleAnchorDataHash2 :: String
exampleAnchorDataHash = "de38a4f5b8b9d8372386cc923bad19d1a0662298cf355bbe947e5eedf127fa9c"
exampleAnchorDataHash2 = "8b4fda934272320ec8d11ba5a7904ab74686a8ec97f2c1331b68d11e28bda26f"

exampleDRepRegAnchorHash, exampleGovActionAnchorHash1, exampleGovActionAnchorHash2 :: String
exampleDRepRegAnchorHash = "15a1c724bfbb8c0a0e84e2e359525017aefb8914ecc219bb9ad469368f14975d"
exampleGovActionAnchorHash1 = "d0c923d899917cd62cbf0e766cc2534a1f4739af69da0241a4992ad24d861ff0"
exampleGovActionAnchorHash2 = "21dd5a8219936e0d756f44f7f1a7179806b5afa45b6cbfb9e7d7efe3123c8e51"

-- ** Paths to the example files for golden tests

exampleAnchorDataPathGolden, exampleAnchorDataPathGolden2 :: String
exampleAnchorDataPathGolden = "test/cardano-cli-golden/files/input/example_anchor_data.txt"
exampleAnchorDataPathGolden2 = "test/cardano-cli-golden/files/input/example_anchor_data2.txt"

exampleDRepRegAnchorPathGolden
  , exampleGovActionAnchorPathGolden1
  , exampleGovActionAnchorPathGolden2
    :: String
exampleDRepRegAnchorPathGolden = "test/cardano-cli-golden/files/input/example_drep_reg_anchor_data.json"
exampleGovActionAnchorPathGolden1 = "test/cardano-cli-golden/files/input/example_gov_action_anchor1.json"
exampleGovActionAnchorPathGolden2 = "test/cardano-cli-golden/files/input/example_gov_action_anchor2.json"

-- ** Paths to the example files for normal testes

exampleAnchorDataPathTest, exampleAnchorDataPathTest2 :: String
exampleAnchorDataPathTest = "test/cardano-cli-test/files/input/example_anchor_data.txt"
exampleAnchorDataPathTest2 = "test/cardano-cli-test/files/input/example_anchor_data2.txt"

exampleDRepRegAnchorPathTest
  , exampleGovActionAnchorPathTest1
  , exampleGovActionAnchorPathTest2
    :: String
exampleDRepRegAnchorPathTest = "test/cardano-cli-test/files/input/example_drep_reg_anchor_data.json"
exampleGovActionAnchorPathTest1 = "test/cardano-cli-test/files/input/example_gov_action_anchor1.json"
exampleGovActionAnchorPathTest2 = "test/cardano-cli-test/files/input/example_gov_action_anchor2.json"

-- ** Ipfs hashes for the example files

exampleAnchorDataIpfsHash, exampleAnchorDataIpfsHash2 :: String
exampleAnchorDataIpfsHash = "QmbL5EBFJLf8DdPkWAskG3Euin9tHY8naqQ2JDoHnWHHXJ"
exampleAnchorDataIpfsHash2 = "QmdTJ4PabgSabg8K1Z4MNXnSVM8bjJnAikC3rVWfPVExQj"

exampleDRepRegAnchorIpfsHash
  , exampleGovActionAnchorIpfsHash1
  , exampleGovActionAnchorIpfsHash2
    :: String
exampleDRepRegAnchorIpfsHash = "Qmb6vATFuc2o9zeFEseTvKZiqGRvuYoZRT5SNZN6L88Les"
exampleGovActionAnchorIpfsHash1 = "QmdGH8Qa1f5mJJxbjYuHUqvwqCqUQm66y7EVNFEMgzZJeA"
exampleGovActionAnchorIpfsHash2 = "QmfCgYdiMSTTJ4Uw93vqwDw2DL2xfr7QrNeRnsxZqaCcLx"

-- | Tamper with the base16 hash by adding one to the first character
tamperBase16Hash :: String -> Maybe String
tamperBase16Hash [] = Nothing
tamperBase16Hash (headChar : tailStr) =
  fmap
    (\i -> hexChars !! ((i + 1) `mod` length hexChars) : lowerCaseRest)
    (elemIndex lowerCaseHead hexChars)
 where
  lowerCaseHead = toLower headChar
  lowerCaseRest = map toLower tailStr
  hexChars = ['0' .. '9'] ++ ['a' .. 'f']

-- | Takes a relative url (as a list of segments), a file path, and an action, and it serves
-- the file in the url provided in a random free port that is passed as a parameter to the
-- action. After the action returns, it shuts down the server. It returns the result of the
-- action. It also ensures the server is shut down even if the action throws an exception.
serveFilesWhile
  :: (MonadBaseControl IO m, MonadTest m, MonadIO m, HasCallStack)
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
