{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Metadata.StakePoolMetadata where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text.IO as Text

import Test.Cardano.CLI.Util as OP

import Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

hprop_golden_stakePoolMetadataHash :: Property
hprop_golden_stakePoolMetadataHash = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  referenceStakePoolMetadata <-
    noteInputFile "test/cardano-cli-golden/files/golden/shelley/metadata/stake_pool_metadata_hash"

  stakePoolMetadataFile <- noteTempFile tempDir "stake-pool-metadata.json"
  outputStakePoolMetadataHashFp <- noteTempFile tempDir "stake-pool-metadata-hash.txt"

  -- Write the example stake pool metadata to disk
  H.evalIO $ Text.writeFile stakePoolMetadataFile exampleStakePoolMetadata

  -- Hash the stake pool metadata
  void $
    execCardanoCLI
      [ "stake-pool"
      , "metadata-hash"
      , "--pool-metadata-file"
      , stakePoolMetadataFile
      , "--out-file"
      , outputStakePoolMetadataHashFp
      ]

  -- Check that the stake pool metadata hash file content is correct.
  expectedStakePoolMetadataHash <- H.readFile referenceStakePoolMetadata
  actualStakePoolMetadataHash <- H.readFile outputStakePoolMetadataHashFp

  equivalence expectedStakePoolMetadataHash actualStakePoolMetadataHash
 where
  exampleStakePoolMetadata :: Text
  exampleStakePoolMetadata =
    "{\"homepage\":\"https://iohk.io\",\"name\":\"Genesis Pool C\",\"ticker\":\"GPC\",\"description\":\"Lorem Ipsum Dolor Sit Amet.\"}"
