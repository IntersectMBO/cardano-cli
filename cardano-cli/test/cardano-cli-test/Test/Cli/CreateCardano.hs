{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cli.CreateCardano where

import Control.Monad (void)
import System.FilePath ((</>))

import Test.Cardano.CLI.Util (execCardanoCLI, watchdogProp)

import Hedgehog (Property)
import Hedgehog.Extras (moduleWorkspace, propertyOnce)
import Hedgehog.Extras qualified as H

-- Execute this test with:
-- @cabal test cardano-cli-test --test-options '-p "/create cardano/'@
hprop_create_cardano :: Property
hprop_create_cardano =
  watchdogProp . propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
    let outputDir = tempDir </> "out"
        eras = ["byron", "shelley", "alonzo", "conway"]
        templates =
          concatMap
            ( \era ->
                [ "--" ++ era ++ "-template"
                , "test/cardano-cli-test/files/input/conway/create-cardano/genesis." ++ era ++ ".spec.json"
                ]
            )
            eras

    void $
      execCardanoCLI $
        [ "conway"
        , "genesis"
        , "create-cardano"
        , "--genesis-dir"
        , outputDir
        , "--testnet-magic"
        , "734"
        ]
          ++ templates

    H.assertFilesExist $ [outputDir </> (era ++ "-genesis.json") | era <- eras]
