{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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
  , NoScriptDatum (..)
  , WitnessableItem (..)
  )
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.AnyScriptWitness
import Cardano.Api.Experimental.Plutus qualified as Exp

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Read
import Cardano.CLI.Type.Common (AnySLanguage (..))

readWithdrawalScriptWitness
  :: forall e era
   . IsEra era
  => (StakeAddress, Coin, Maybe (ScriptRequirements WithdrawalItem))
  -> CIO e (StakeAddress, Coin, AnyWitness (LedgerEra era))
readWithdrawalScriptWitness (stakeAddr, withdrawalAmt, Nothing) =
  return (stakeAddr, withdrawalAmt, Exp.AnyKeyWitnessPlaceholder)
readWithdrawalScriptWitness (stakeAddr, withdrawalAmt, Just certScriptReq) =
  case certScriptReq of
    OnDiskSimpleScript scriptFp -> do
      let sFp = unFile scriptFp
      sWit <- AnySimpleScriptWitness . Exp.SScript <$> readFileSimpleScript sFp (Exp.useEra @era)

      return
        ( stakeAddr
        , withdrawalAmt
        , sWit
        )
    OnDiskPlutusScript (OnDiskPlutusScriptCliArgs scriptFp NoScriptDatumAllowed redeemerFile execUnits) -> do
      let plutusScriptFp = unFile scriptFp
      Exp.AnyPlutusScript script <-
        readFilePlutusScript @_ @era plutusScriptFp
      redeemer <-
        fromExceptTCli $
          readScriptDataOrFile redeemerFile

      let lang = Exp.plutusScriptInEraSLanguage script
          pScript = Exp.PScript script
          sw =
            Exp.PlutusScriptWitness
              lang
              pScript
              Exp.NoScriptDatum
              redeemer
              execUnits
      return
        ( stakeAddr
        , withdrawalAmt
        , AnyPlutusScriptWitness $
            AnyPlutusCertifyingScriptWitness sw
        )
    SimpleReferenceScript (SimpleRefScriptArgs refTxIn NoPolicyId) ->
      return
        ( stakeAddr
        , withdrawalAmt
        , AnySimpleScriptWitness $ Exp.SReferenceScript refTxIn
        )
    PlutusReferenceScript
      ( PlutusRefScriptCliArgs
          refTxIn
          (AnySLanguage lang)
          NoScriptDatumAllowed
          NoPolicyId
          redeemerFile
          execUnits
        ) -> do
        redeemer <-
          fromExceptTCli $
            readScriptDataOrFile redeemerFile
        let sWit =
              Exp.AnyPlutusScriptWitness $
                AnyPlutusCertifyingScriptWitness $
                  Exp.PlutusScriptWitness
                    lang
                    (Exp.PReferenceScript refTxIn)
                    Exp.NoScriptDatum
                    redeemer
                    execUnits
        return
          ( stakeAddr
          , withdrawalAmt
          , sWit
          )
