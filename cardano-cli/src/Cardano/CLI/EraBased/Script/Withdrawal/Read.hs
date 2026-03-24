{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Script.Withdrawal.Read
  ( readWithdrawalScriptWitness
  )
where

import Cardano.Api
import Cardano.Api.Experimental
  ( AnyWitness (..)
  , IsEra (..)
  , LedgerEra
  )
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.AnyScriptWitness
import Cardano.Api.Experimental.Plutus qualified as Exp.Plutus

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Read
import Cardano.CLI.Type.Common (AnySLanguage (..))

readWithdrawalScriptWitness
  :: forall e era
   . IsEra era
  => (StakeAddress, Coin, Maybe AnyNonAssetScript)
  -> CIO e (StakeAddress, Coin, AnyWitness (LedgerEra era))
readWithdrawalScriptWitness (stakeAddr, withdrawalAmt, Nothing) =
  return (stakeAddr, withdrawalAmt, Exp.AnyKeyWitnessPlaceholder)
readWithdrawalScriptWitness (stakeAddr, withdrawalAmt, Just certScriptReq) =
  case certScriptReq of
    AnyNonAssetScriptSimple simpleReq ->
      case simpleReq of
        OnDiskSimpleScript scriptFp -> do
          let sFp = unFile scriptFp
          sWit <- AnySimpleScriptWitness . Exp.SScript <$> readFileSimpleScript sFp (Exp.useEra @era)
          return (stakeAddr, withdrawalAmt, sWit)
        ReferenceSimpleScript refTxIn ->
          return
            ( stakeAddr
            , withdrawalAmt
            , AnySimpleScriptWitness $ Exp.SReferenceScript refTxIn
            )
    AnyNonAssetScriptPlutus plutusReq ->
      case plutusReq of
        OnDiskPlutusNonAssetScript scriptFp redeemerFile execUnits -> do
          let plutusScriptFp = unFile scriptFp
          Exp.Plutus.AnyPlutusScript script <-
            readFilePlutusScript @_ @era plutusScriptFp
          redeemer <-
            fromExceptTCli $
              readScriptDataOrFile redeemerFile

          let lang = Exp.Plutus.plutusScriptInEraSLanguage script
              pScript = Exp.Plutus.PScript script
              sw =
                Exp.Plutus.PlutusScriptWitness
                  lang
                  pScript
                  Exp.Plutus.NoScriptDatum
                  redeemer
                  execUnits
          return
            ( stakeAddr
            , withdrawalAmt
            , AnyPlutusScriptWitness $
                AnyPlutusCertifyingScriptWitness sw
            )
        ReferencePlutusNonAssetScript refTxIn (AnySLanguage lang) redeemerFile execUnits -> do
          redeemer <-
            fromExceptTCli $
              readScriptDataOrFile redeemerFile
          let sWit =
                Exp.AnyPlutusScriptWitness $
                  AnyPlutusCertifyingScriptWitness $
                    Exp.Plutus.PlutusScriptWitness
                      lang
                      (Exp.Plutus.PReferenceScript refTxIn)
                      Exp.Plutus.NoScriptDatum
                      redeemer
                      execUnits
          return
            ( stakeAddr
            , withdrawalAmt
            , sWit
            )
