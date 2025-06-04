{-# LANGUAGE FlexibleContexts #-}

module Test.Cardano.CLI.Hash
  ( exampleAnchorDataHash
  , exampleAnchorDataHash2
  , exampleAnchorDataPathTest
  , exampleAnchorDataPathTest2
  , exampleAnchorDataPathGolden
  , exampleAnchorDataPathGolden2
  , exampleAnchorDataIpfsHash
  , exampleAnchorDataIpfsHash2
  , tamperBase16Hash
  )
where

import Data.Char (toLower)
import Data.List (elemIndex)

exampleAnchorDataHash, exampleAnchorDataHash2 :: String
exampleAnchorDataHash = "de38a4f5b8b9d8372386cc923bad19d1a0662298cf355bbe947e5eedf127fa9c"
exampleAnchorDataHash2 = "8b4fda934272320ec8d11ba5a7904ab74686a8ec97f2c1331b68d11e28bda26f"

exampleAnchorDataPathGolden, exampleAnchorDataPathGolden2 :: String
exampleAnchorDataPathGolden = "test/cardano-cli-golden/files/input/example_anchor_data.txt"
exampleAnchorDataPathGolden2 = "test/cardano-cli-golden/files/input/example_anchor_data2.txt"

exampleAnchorDataPathTest, exampleAnchorDataPathTest2 :: String
exampleAnchorDataPathTest = "test/cardano-cli-test/files/input/example_anchor_data.txt"
exampleAnchorDataPathTest2 = "test/cardano-cli-golden/files/input/example_anchor_data2.txt"

exampleAnchorDataIpfsHash, exampleAnchorDataIpfsHash2 :: String
exampleAnchorDataIpfsHash = "QmbL5EBFJLf8DdPkWAskG3Euin9tHY8naqQ2JDoHnWHHXJ"
exampleAnchorDataIpfsHash2 = "QmdTJ4PabgSabg8K1Z4MNXnSVM8bjJnAikC3rVWfPVExQj"

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
