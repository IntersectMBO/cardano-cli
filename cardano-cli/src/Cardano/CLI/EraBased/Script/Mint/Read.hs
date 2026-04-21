{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Cardano.Api.Experimental.Plutus qualified as Exp.Plutus
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
  => AnyMintScript -> CIO e (PolicyId, AnyScriptWitness (Exp.LedgerEra era))
readMintScriptWitness (AnyMintScriptSimpleOnDisk scriptFp) = do
  let sFp = unFile scriptFp
  s <- readFileSimpleScript sFp (Exp.useEra @era)
  let sHash :: L.ScriptHash =
        Exp.hashSimpleScript (s :: Exp.SimpleScript (Exp.LedgerEra era))
  return (fromMaryPolicyID $ L.PolicyID sHash, AnyScriptWitnessSimple $ Exp.SScript s)
readMintScriptWitness (AnyMintScriptSimpleRef refTxIn polId) =
  return (polId, AnyScriptWitnessSimple $ Exp.SReferenceScript refTxIn)
readMintScriptWitness (AnyMintScriptPlutus plutusReq) =
  case plutusReq of
    OnDiskPlutusMintingScript scriptFp redeemerFile execUnits -> do
      let plutusScriptFp = unFile scriptFp
      Exp.Plutus.AnyPlutusScript script <-
        readFilePlutusScript @_ @era plutusScriptFp
      let polId = fromMaryPolicyID . L.PolicyID $ Exp.Plutus.hashPlutusScriptInEra script
      redeemer <-
        fromExceptTCli $
          readScriptDataOrFile redeemerFile
      let pScript = Exp.Plutus.PScript script
          lang = Exp.Plutus.plutusScriptInEraSLanguage script
          sw =
            Exp.Plutus.PlutusScriptWitness
              lang
              pScript
              Exp.Plutus.NoScriptDatum
              redeemer
              execUnits
      return
        ( polId
        , AnyScriptWitnessPlutus $
            AnyPlutusMintingScriptWitness sw
        )
    ReferencePlutusMintingScript refTxIn (AnySLanguage lang) polId redeemerFile execUnits -> do
      redeemer <-
        fromExceptTCli $ readScriptDataOrFile redeemerFile
      let sw =
            Exp.Plutus.PlutusScriptWitness
              lang
              (Exp.Plutus.PReferenceScript refTxIn)
              Exp.Plutus.NoScriptDatum
              redeemer
              execUnits
      return
        ( polId
        , AnyScriptWitnessPlutus $
            AnyPlutusMintingScriptWitness
              sw
        )
