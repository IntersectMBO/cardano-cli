module Test.Cli.Hash where

import           Control.Monad (void)
import           Data.List (intercalate)
import           GHC.IO.Exception (ExitCode (ExitFailure))
import           System.Directory (getCurrentDirectory)
import           System.FilePath (dropTrailingPathSeparator)
import           System.FilePath.Posix (splitDirectories)

import           Test.Cardano.CLI.Util

import           Hedgehog as H
import qualified Hedgehog.Extras as H

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
