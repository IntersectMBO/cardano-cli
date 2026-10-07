{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Script.Receiving.Read
  ( readReceivingScriptWitnesses
  )
where

import Cardano.Api hiding (AnyScriptWitness)
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.AnyScriptWitness
import Cardano.Api.Experimental.Plutus qualified as Plutus
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Read
import Cardano.CLI.Type.Common (AnySLanguage (..))

import Control.Monad (unless, when)
import Data.Map.Strict qualified as Map
import Data.Word (Word32)

-- Each entry authorizes one protected output at its original body index. Reject
-- duplicate indexes rather than silently selecting a redeemer or budget.
readReceivingScriptWitnesses
  :: forall era e
   . Exp.IsEra era
  => [(Word32, AnyNonAssetScript)]
  -> CIO e (Map.Map Word32 (AnyScriptWitness (Exp.LedgerEra era)))
readReceivingScriptWitnesses requirements = do
  when (length indexes /= Map.size (Map.fromList [(outputIndex, ()) | outputIndex <- indexes])) $
    throwCliError @String
      "Duplicate --receiving-output-index: each protected script output has one Receiving witness."
  validateEra
  Map.fromList <$> mapM readWitness requirements
 where
  validateEra :: CIO e ()
  validateEra = case Exp.useEra @era of
    Exp.ConwayEra ->
      unless (null requirements) $ throwCliError @String "Receiving witnesses require the Dijkstra era."
    Exp.DijkstraEra -> pure ()
  indexes = map fst requirements
  readWitness (outputIndex, requirement) = do
    witness <- case requirement of
      AnyNonAssetScriptSimple (OnDiskSimpleScript scriptFile) ->
        AnyScriptWitnessSimple . Exp.SScript <$> readFileSimpleScript (unFile scriptFile) (Exp.useEra @era)
      AnyNonAssetScriptSimple (ReferenceSimpleScript txIn) ->
        pure (AnyScriptWitnessSimple (Exp.SReferenceScript txIn))
      AnyNonAssetScriptPlutus (OnDiskPlutusNonAssetScript scriptFile redeemerFile units) -> do
        Plutus.AnyPlutusScript script <- readFilePlutusScript @_ @era (unFile scriptFile)
        redeemer <- fromExceptTCli (readScriptDataOrFile redeemerFile)
        case Plutus.plutusScriptInEraSLanguage script of
          L.SPlutusV4 ->
            pure $
              AnyScriptWitnessPlutus $
                AnyPlutusReceivingScriptWitness $
                  Plutus.PlutusScriptWitness L.SPlutusV4 (Plutus.PScript script) Plutus.NoScriptDatum redeemer units
          _ ->
            throwCliError @String
              "Receiving scripts require Plutus V4; the supplied script uses an older language."
      AnyNonAssetScriptPlutus
        (ReferencePlutusNonAssetScript txIn (AnySLanguage language) redeemerFile units) ->
          case language of
            L.SPlutusV4 -> do
              redeemer <- fromExceptTCli (readScriptDataOrFile redeemerFile)
              pure $
                AnyScriptWitnessPlutus $
                  AnyPlutusReceivingScriptWitness $
                    Plutus.PlutusScriptWitness
                      L.SPlutusV4
                      (Plutus.PReferenceScript txIn)
                      Plutus.NoScriptDatum
                      redeemer
                      units
            _ -> throwCliError @String "Receiving reference scripts require Plutus V4."
    pure (outputIndex, witness)
