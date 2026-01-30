{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Script.Mint.Read
  ( readMintScriptWitness
  )
where

import Cardano.Api hiding (AnyScriptWitness)
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.AnyScriptWitness
import Cardano.Api.Experimental.Plutus qualified as Exp
import Cardano.Api.Experimental.Plutus qualified as L
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Read
import Cardano.CLI.Type.Common (AnySLanguage (..))
import Cardano.Ledger.Core qualified as L

readMintScriptWitness
  :: forall era e
   . Exp.IsEra era
  => ScriptRequirements Exp.MintItem -> CIO e (PolicyId, AnyScriptWitness (Exp.LedgerEra era))
readMintScriptWitness (OnDiskSimpleScript scriptFp) = do
  let sFp = unFile scriptFp
  s <- readFileSimpleScript sFp (Exp.useEra @era)
  let sHash :: L.ScriptHash =
        Exp.hashSimpleScript (s :: Exp.SimpleScript (Exp.LedgerEra era))
  return (fromMaryPolicyID $ L.PolicyID sHash, AnyScriptWitnessSimple $ Exp.SScript s)
readMintScriptWitness
  ( OnDiskPlutusScript
      (OnDiskPlutusScriptCliArgs scriptFp Exp.NoScriptDatumAllowed redeemerFile execUnits)
    ) = do
    let plutusScriptFp = unFile scriptFp
    Exp.AnyPlutusScript script <-
      readFilePlutusScript @_ @era plutusScriptFp
    let polId = fromMaryPolicyID . L.PolicyID $ L.hashPlutusScriptInEra script
    redeemer <-
      fromExceptTCli $
        readScriptDataOrFile redeemerFile

    let pScript = Exp.PScript script
        lang = Exp.plutusScriptInEraSLanguage script
    let sw =
          Exp.PlutusScriptWitness
            lang
            pScript
            Exp.NoScriptDatum
            redeemer
            execUnits
    return
      ( polId
      , AnyScriptWitnessPlutus $
          AnyPlutusMintingScriptWitness sw
      )
readMintScriptWitness
  ( PlutusReferenceScript
      ( PlutusRefScriptCliArgs
          refTxIn
          (AnySLanguage lang)
          Exp.NoScriptDatumAllowed
          polId
          redeemerFile
          execUnits
        )
    ) = do
    redeemer <-
      fromExceptTCli $ readScriptDataOrFile redeemerFile

    let sw =
          Exp.PlutusScriptWitness
            lang
            (Exp.PReferenceScript refTxIn)
            Exp.NoScriptDatum
            redeemer
            execUnits
    return
      ( polId
      , AnyScriptWitnessPlutus $
          AnyPlutusMintingScriptWitness
            sw
      )
readMintScriptWitness (SimpleReferenceScript (SimpleRefScriptArgs refTxIn polId)) =
  return (polId, AnyScriptWitnessSimple $ Exp.SReferenceScript refTxIn)
