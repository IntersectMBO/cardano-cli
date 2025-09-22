{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Legacy.Genesis.Create where

import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Plutus.CostModels (costModelsValid, getCostModelParams)
import Cardano.Ledger.Plutus.Language

import Control.Monad (void)
import Data.Map.Strict qualified as Map
import System.FilePath ((</>))

import Test.Cardano.CLI.Util (execCardanoCLI, propertyOnce, watchdogProp)

import Hedgehog (Property)
import Hedgehog qualified as H
import Hedgehog.Extras.Test qualified as H

{- HLINT ignore "Use camelCase" -}

-- | QA needs the ability to generate a V2 cost model with 175 parameters in the Alonzo era
hprop_golden_alonzo_genesis_v2_cost_model_has_175_parameters :: Property
hprop_golden_alonzo_genesis_v2_cost_model_has_175_parameters =
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
    AlonzoGenesis _ costModels _ _ _ _ _ _ <- H.readJsonFileOk alonzoGenesisFp
    let v2CostModel = costModelsValid costModels
        mV2Params = Map.lookup PlutusV2 v2CostModel
    v2Params <- getCostModelParams <$> H.evalMaybe mV2Params

    H.note_ $ "Cost model filepath: " <> alonzoGenesisFp
    length v2Params H.=== 175
