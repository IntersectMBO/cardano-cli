{-# LANGUAGE FlexibleContexts #-}

module Test.Cardano.CLI.Hash
  ( AnchorDataExample (..)
  , serveFilesWhile
  , tamperBase16Hash
  , dummyAnchorDataExample1
  , dummyAnchorDataExample2
  , drepRegAnchorDataExample
  , govActionAnchorDataExample1
  , govActionAnchorDataExample2
  , tamperAnchorDataExampleHash
  , stakePoolMetadataExample
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

-- * Anchor data examples

-- | Groups information about a given anchor data example
data AnchorDataExample = AnchorDataExample
  { anchorDataHash :: String
  , anchorDataPathGolden :: String
  , anchorDataPathTest :: String
  , anchorDataIpfsHash :: String
  }
  deriving (Eq, Show)

-- ** Examples

dummyAnchorDataExample1 :: AnchorDataExample
dummyAnchorDataExample1 =
  AnchorDataExample
    { anchorDataHash = "de38a4f5b8b9d8372386cc923bad19d1a0662298cf355bbe947e5eedf127fa9c"
    , anchorDataPathGolden = "test/cardano-cli-golden/files/input/example_anchor_data1.txt"
    , anchorDataPathTest = "test/cardano-cli-test/files/input/example_anchor_data1.txt"
    , anchorDataIpfsHash = "QmbL5EBFJLf8DdPkWAskG3Euin9tHY8naqQ2JDoHnWHHXJ"
    }

dummyAnchorDataExample2 :: AnchorDataExample
dummyAnchorDataExample2 =
  AnchorDataExample
    { anchorDataHash = "8b4fda934272320ec8d11ba5a7904ab74686a8ec97f2c1331b68d11e28bda26f"
    , anchorDataPathGolden = "test/cardano-cli-golden/files/input/example_anchor_data2.txt"
    , anchorDataPathTest = "test/cardano-cli-test/files/input/example_anchor_data2.txt"
    , anchorDataIpfsHash = "QmdTJ4PabgSabg8K1Z4MNXnSVM8bjJnAikC3rVWfPVExQj"
    }

drepRegAnchorDataExample :: AnchorDataExample
drepRegAnchorDataExample =
  AnchorDataExample
    { anchorDataHash = "15a1c724bfbb8c0a0e84e2e359525017aefb8914ecc219bb9ad469368f14975d"
    , anchorDataPathGolden = "test/cardano-cli-golden/files/input/example_drep_reg_anchor_data.json"
    , anchorDataPathTest = "test/cardano-cli-test/files/input/example_drep_reg_anchor_data.json"
    , anchorDataIpfsHash = "Qmb6vATFuc2o9zeFEseTvKZiqGRvuYoZRT5SNZN6L88Les"
    }

govActionAnchorDataExample1 :: AnchorDataExample
govActionAnchorDataExample1 =
  AnchorDataExample
    { anchorDataHash = "d0c923d899917cd62cbf0e766cc2534a1f4739af69da0241a4992ad24d861ff0"
    , anchorDataPathGolden = "test/cardano-cli-golden/files/input/example_gov_action_anchor1.json"
    , anchorDataPathTest = "test/cardano-cli-test/files/input/example_gov_action_anchor1.json"
    , anchorDataIpfsHash = "QmdGH8Qa1f5mJJxbjYuHUqvwqCqUQm66y7EVNFEMgzZJeA"
    }

govActionAnchorDataExample2 :: AnchorDataExample
govActionAnchorDataExample2 =
  AnchorDataExample
    { anchorDataHash = "21dd5a8219936e0d756f44f7f1a7179806b5afa45b6cbfb9e7d7efe3123c8e51"
    , anchorDataPathGolden = "test/cardano-cli-golden/files/input/example_gov_action_anchor2.json"
    , anchorDataPathTest = "test/cardano-cli-test/files/input/example_gov_action_anchor2.json"
    , anchorDataIpfsHash = "QmfCgYdiMSTTJ4Uw93vqwDw2DL2xfr7QrNeRnsxZqaCcLx"
    }

stakePoolMetadataExample :: AnchorDataExample
stakePoolMetadataExample =
  AnchorDataExample
    { anchorDataHash = "8241de08075886a7d09c847c9bbd1719459dac0bd0a2f085e673611ebb9a5965"
    , anchorDataPathGolden = "test/cardano-cli-golden/files/input/example_stake_pool_metadata.json"
    , anchorDataPathTest = "test/cardano-cli-test/files/input/example_stake_pool_metadata.json"
    , anchorDataIpfsHash = "QmR1HAT4Hb4HjjqcgoXwupYXMF6t8h7MoSP24HMfV8t38a"
    }

-- * Utility functions

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

-- | Tamper with the base16 hash in an AnchorDataExample by adding one to the first character
tamperAnchorDataExampleHash :: AnchorDataExample -> Maybe AnchorDataExample
tamperAnchorDataExampleHash anchorDataExample@(AnchorDataExample{anchorDataHash = anchorDataHash'}) = do
  tamperedAnchorDataHash <- tamperBase16Hash anchorDataHash'
  return (anchorDataExample{anchorDataHash = tamperedAnchorDataHash})

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
