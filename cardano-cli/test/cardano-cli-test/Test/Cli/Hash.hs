{-# LANGUAGE FlexibleContexts #-}

module Test.Cli.Hash where

import           Control.Monad (void)
import           Control.Monad.IO.Class
import           Data.List (intercalate)
import           GHC.IO.Exception (ExitCode (..))
import           System.Directory (getCurrentDirectory)
import           System.FilePath (dropTrailingPathSeparator)
import           System.FilePath.Posix (splitDirectories)

import           Test.Cardano.CLI.Hash (AnchorDataExample (..), dummyAnchorDataExample1,
                   serveFilesWhile)
import           Test.Cardano.CLI.Util

import           Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/generate anchor data hash from file/"'@
hprop_generate_anchor_data_hash_from_file :: Property
hprop_generate_anchor_data_hash_from_file =
  propertyOnce $ do
    let anchorDataExample = dummyAnchorDataExample1
    result <-
      execCardanoCLI
        [ "hash"
        , "anchor-data"
        , "--file-binary"
        , anchorDataPathTest anchorDataExample
        ]
    result === anchorDataHash anchorDataExample

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/check anchor data hash from file/"'@
hprop_check_anchor_data_hash_from_file :: Property
hprop_check_anchor_data_hash_from_file =
  propertyOnce $ do
    let anchorDataExample = dummyAnchorDataExample1
    void $
      execCardanoCLI
        [ "hash"
        , "anchor-data"
        , "--file-binary"
        , anchorDataPathTest anchorDataExample
        , "--expected-hash"
        , anchorDataHash anchorDataExample
        ]

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/check anchor data hash from file fails/"'@
hprop_check_anchor_data_hash_from_file_fails :: Property
hprop_check_anchor_data_hash_from_file_fails =
  propertyOnce $ do
    let anchorDataExample = dummyAnchorDataExample1
    (ec, _, _) <-
      execDetailCardanoCLI
        [ "hash"
        , "anchor-data"
        , "--file-binary"
        , anchorDataPathTest anchorDataExample
        , "--expected-hash"
        , 'c' : drop 1 (anchorDataHash anchorDataExample)
        ]
    ec === ExitFailure 1

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/generate anchor data hash from file uri/"'@
hprop_generate_anchor_data_hash_from_file_uri :: Property
hprop_generate_anchor_data_hash_from_file_uri =
  propertyOnce $ do
    let anchorDataExample = dummyAnchorDataExample1
    cwd <- H.evalIO getCurrentDirectory
    posixCwd <- toPOSIX cwd
    result <-
      execCardanoCLI
        [ "hash"
        , "anchor-data"
        , "--url"
        , "file://" ++ posixCwd ++ "/" ++ anchorDataPathTest anchorDataExample
        ]
    result === anchorDataHash anchorDataExample
 where
  toPOSIX :: MonadIO m => FilePath -> PropertyT m [Char]
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
    let anchorDataExample = dummyAnchorDataExample1
    let relativeUrl = ["example", "url", "file.txt"]

    -- Create temporary HTTP server with files required by the call to `cardano-cli`
    serveFilesWhile
      [(relativeUrl, anchorDataPathTest anchorDataExample)]
      ( \port -> do
          void $
            execCardanoCLI
              [ "hash"
              , "anchor-data"
              , "--url"
              , "http://localhost:" ++ show port ++ "/" ++ intercalate "/" relativeUrl
              , "--expected-hash"
              , anchorDataHash anchorDataExample
              ]
      )

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/check anchor data hash from ipfs uri/"'@
hprop_check_anchor_data_hash_from_ipfs_uri :: Property
hprop_check_anchor_data_hash_from_ipfs_uri =
  propertyOnce $ do
    let anchorDataExample = dummyAnchorDataExample1
    let relativeUrl = ["ipfs", anchorDataIpfsHash anchorDataExample]

    -- Create temporary HTTP server with files required by the call to `cardano-cli`
    -- In this case, the server emulates an IPFS gateway
    serveFilesWhile
      [(relativeUrl, anchorDataPathTest anchorDataExample)]
      ( \port -> do
          void $
            execCardanoCLIWithEnvVars
              [("IPFS_GATEWAY_URI", "http://localhost:" ++ show port ++ "/")]
              [ "hash"
              , "anchor-data"
              , "--url"
              , "ipfs://" ++ anchorDataIpfsHash anchorDataExample
              , "--expected-hash"
              , anchorDataHash anchorDataExample
              ]
      )
