{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Legacy.Genesis.Create where

import Cardano.Api.Ledger (AlonzoGenesis (AlonzoGenesis), getCostModelParams)

import Control.Monad (void)
import System.FilePath ((</>))

import Test.Cardano.CLI.Util (execCardanoCLI, propertyOnce, watchdogProp)

import Hedgehog (Property)
import Hedgehog qualified as H
import Hedgehog.Extras.Test qualified as H

-- | Verify that the generated Alonzo genesis contains a PlutusV1 cost model with 166 parameters.
-- AlonzoGenesis only stores a PlutusV1 cost model. PlutusV2 cost models are not part of any
-- genesis file - they are introduced via protocol parameter updates.
hprop_golden_alonzo_genesis_v1_cost_model_has_166_parameters :: Property
hprop_golden_alonzo_genesis_v1_cost_model_has_166_parameters =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    alonzoGenesisSpec <-
      H.note "test/cardano-cli-golden/files/input/genesis.alonzo.spec.json"
    shelleyGenesisSpec <-
      H.note "test/cardano-cli-golden/files/input/genesis.spec.json"

    conwayGenesisSpec <-
      H.note "test/cardano-cli-golden/files/input/genesis.conway.spec.json"

    outDir <- H.note $ tempDir </> "genesis-dir"

    -- Copy genesis spec files to genesis output directory
    alonzoGenSpecBs <- H.readFile alonzoGenesisSpec
    shelleyGenSpecBs <- H.readFile shelleyGenesisSpec
    conwayGenesisSpecBs <- H.readFile conwayGenesisSpec

    H.createDirectoryIfMissing_ outDir
    genAlonzoSpec <- H.noteTempFile outDir "genesis.alonzo.spec.json"
    genConwaySpec <- H.noteTempFile outDir "genesis.conway.spec.json"
    genShelleySpec <- H.noteTempFile outDir "genesis.spec.json"

    H.writeFile genAlonzoSpec alonzoGenSpecBs
    H.writeFile genConwaySpec conwayGenesisSpecBs
    H.writeFile genShelleySpec shelleyGenSpecBs

    -- Create genesis files
    void $
      execCardanoCLI
        [ "legacy"
        , "genesis"
        , "create"
        , "--alonzo"
        , "--genesis-dir"
        , outDir
        , "--mainnet"
        ]

    -- Read generated alonzo genesis file
    alonzoGenesisFp <- H.note $ outDir </> "genesis.alonzo.json"
    AlonzoGenesis _ v1CostModel _ _ _ _ _ _ _extraConfig <- H.readJsonFileOk alonzoGenesisFp
    let v1Params = getCostModelParams v1CostModel
    H.note_ $ "Cost model filepath: " <> alonzoGenesisFp
    length v1Params H.=== 166
