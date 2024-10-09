{-# LANGUAGE GADTs #-}

module Test.Cli.AddCostModels where

import           Cardano.Api
import           Cardano.Api.Ledger (StrictMaybe (..))
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.ProtocolParameters

import           Cardano.CLI.EraBased.Run.Governance.Actions

import           Test.Gen.Cardano.Api.ProtocolParameters
import           Test.Gen.Cardano.Api.Typed

import           Hedgehog

hprop_roundtrip_Alonzo_addCostModelsToEraBasedProtocolParametersUpdate :: Property
hprop_roundtrip_Alonzo_addCostModelsToEraBasedProtocolParametersUpdate =
  property $ do
    ppu <- forAll genAlonzoEraBasedProtocolParametersUpdate
    cmdl <- forAll genCostModels
    tripping
      cmdl
      (flip (addCostModelsToEraBasedProtocolParametersUpdate AlonzoEraOnwardsAlonzo) ppu)
      getCostModels
 where
  getCostModels :: EraBasedProtocolParametersUpdate era -> Maybe L.CostModels
  getCostModels (AlonzoEraBasedProtocolParametersUpdate _ _ AlonzoOnwardsPParams{alCostModels = SJust cmdls} _) = Just cmdls
  getCostModels _ = Nothing

hprop_roundtrip_Babbage_addCostModelsToEraBasedProtocolParametersUpdate :: Property
hprop_roundtrip_Babbage_addCostModelsToEraBasedProtocolParametersUpdate =
  property $ do
    ppu <- forAll genBabbageEraBasedProtocolParametersUpdate
    cmdl <- forAll genCostModels
    tripping
      cmdl
      (flip (addCostModelsToEraBasedProtocolParametersUpdate AlonzoEraOnwardsBabbage) ppu)
      getCostModels
 where
  getCostModels :: EraBasedProtocolParametersUpdate era -> Maybe L.CostModels
  getCostModels (BabbageEraBasedProtocolParametersUpdate _ AlonzoOnwardsPParams{alCostModels = SJust cmdls} _ _) = Just cmdls
  getCostModels _ = Nothing

hprop_roundtrip_Conway_addCostModelsToEraBasedProtocolParametersUpdate :: Property
hprop_roundtrip_Conway_addCostModelsToEraBasedProtocolParametersUpdate =
  property $ do
    ppu <- forAll genConwayEraBasedProtocolParametersUpdate
    cmdl <- forAll genCostModels
    tripping
      cmdl
      (flip (addCostModelsToEraBasedProtocolParametersUpdate AlonzoEraOnwardsConway) ppu)
      getCostModels
 where
  getCostModels :: EraBasedProtocolParametersUpdate era -> Maybe L.CostModels
  getCostModels (ConwayEraBasedProtocolParametersUpdate _ AlonzoOnwardsPParams{alCostModels = SJust cmdls} _ _) = Just cmdls
  getCostModels _ = Nothing
