{-# LANGUAGE FlexibleContexts #-}

module Test.Cli.Hash where

import Control.Monad (void)
import Control.Monad.IO.Class
import Data.List (intercalate)
import GHC.IO.Exception (ExitCode (..))
import System.Directory (getCurrentDirectory)
import System.FilePath (dropTrailingPathSeparator)
import System.FilePath.Posix (splitDirectories)

import Test.Cardano.CLI.Hash
  ( exampleAnchorDataHash
  , exampleAnchorDataIpfsHash
  , exampleAnchorDataPathTest
  , serveFilesWhile
  )
import Test.Cardano.CLI.Hedgehog qualified as H
import Test.Cardano.CLI.Util

import Hedgehog as H

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/generate anchor data hash from file/"'@
hprop_generate_anchor_data_hash_from_file :: Property
hprop_generate_anchor_data_hash_from_file =
  watchdogProp . propertyOnce $ do
    result <-
      execCardanoCLI
        [ "hash"
        , "anchor-data"
        , "--file-binary"
        , exampleAnchorDataPathTest
        ]
    result === exampleAnchorDataHash

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/check anchor data hash from file/"'@
hprop_check_anchor_data_hash_from_file :: Property
hprop_check_anchor_data_hash_from_file =
  watchdogProp . propertyOnce $ do
    void $
      execCardanoCLI
        [ "hash"
        , "anchor-data"
        , "--file-binary"
        , exampleAnchorDataPathTest
        , "--expected-hash"
        , exampleAnchorDataHash
        ]

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/check anchor data hash from file fails/"'@
hprop_check_anchor_data_hash_from_file_fails :: Property
hprop_check_anchor_data_hash_from_file_fails =
  watchdogProp . propertyOnce $ do
    (ec, _, _) <-
      execDetailCardanoCLI
        [ "hash"
        , "anchor-data"
        , "--file-binary"
        , exampleAnchorDataPathTest
        , "--expected-hash"
        , 'c' : drop 1 exampleAnchorDataHash
        ]
    ec === ExitFailure 1

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/generate anchor data hash from file uri/"'@
hprop_generate_anchor_data_hash_from_file_uri :: Property
hprop_generate_anchor_data_hash_from_file_uri =
  watchdogProp . propertyOnce $ do
    cwd <- H.evalIO getCurrentDirectory
    posixCwd <- toPOSIX cwd
    result <-
      execCardanoCLI
        [ "hash"
        , "anchor-data"
        , "--url"
        , "file://" ++ posixCwd ++ "/" ++ exampleAnchorDataPathTest
        ]
    result === exampleAnchorDataHash
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
  watchdogProp . propertyOnce $ do
    let relativeUrl = ["example", "url", "file.txt"]

    -- Create temporary HTTP server with files required by the call to `cardano-cli`
    serveFilesWhile
      [(relativeUrl, exampleAnchorDataPathTest)]
      ( \port -> do
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

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/check anchor data hash from ipfs uri/"'@
hprop_check_anchor_data_hash_from_ipfs_uri :: Property
hprop_check_anchor_data_hash_from_ipfs_uri =
  watchdogProp . propertyOnce $ do
    let relativeUrl = ["ipfs", exampleAnchorDataIpfsHash]

    -- Create temporary HTTP server with files required by the call to `cardano-cli`
    -- In this case, the server emulates an IPFS gateway
    serveFilesWhile
      [(relativeUrl, exampleAnchorDataPathTest)]
      ( \port -> do
          void $
            execCardanoCLIWithEnvVars
              [("IPFS_GATEWAY_URI", "http://localhost:" ++ show port ++ "/")]
              [ "hash"
              , "anchor-data"
              , "--url"
              , "ipfs://" ++ exampleAnchorDataIpfsHash
              , "--expected-hash"
              , exampleAnchorDataHash
              ]
      )
