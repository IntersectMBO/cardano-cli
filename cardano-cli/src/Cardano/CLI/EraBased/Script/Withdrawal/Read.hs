{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.EraBased.Script.Withdrawal.Read
  ( readWithdrawalScriptWitness
  )
where

import Cardano.Api
import Cardano.Api.Experimental
  ( IsEra (..)
  , NoScriptDatum (..)
  , WitnessableItem (..)
  )

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.EraBased.Script.Withdrawal.Type (WithdrawalScriptWitness (..))
import Cardano.CLI.Read

readWithdrawalScriptWitness
  :: IsEra era
  => (StakeAddress, Coin, Maybe (ScriptRequirements WithdrawalItem))
  -> CIO e (StakeAddress, Coin, Maybe (WithdrawalScriptWitness era))
readWithdrawalScriptWitness (stakeAddr, withdrawalAmt, Nothing) =
  return (stakeAddr, withdrawalAmt, Nothing)
readWithdrawalScriptWitness (stakeAddr, withdrawalAmt, Just certScriptReq) =
  let sbe = convert useEra
   in case certScriptReq of
        OnDiskSimpleScript scriptFp -> do
          let sFp = unFile scriptFp
          s <-
            readFileSimpleScript sFp
          case s of
            SimpleScript ss -> do
              return
                ( stakeAddr
                , withdrawalAmt
                , Just $
                    WithdrawalScriptWitness
                      ( SimpleScriptWitness (sbeToSimpleScriptLanguageInEra sbe) $
                          SScript ss
                      )
                )
        OnDiskPlutusScript (OnDiskPlutusScriptCliArgs scriptFp NoScriptDatumAllowed redeemerFile execUnits) -> do
          let plutusScriptFp = unFile scriptFp
          plutusScript <-
            readFilePlutusScript plutusScriptFp
          redeemer <-
            fromExceptTCli $
              readScriptDataOrFile redeemerFile
          case plutusScript of
            AnyPlutusScript lang script -> do
              let pScript = PScript script
              sLangSupported <-
                fromMaybeCli
                  ( PlutusScriptWitnessLanguageNotSupportedInEra
                      (AnyPlutusScriptVersion lang)
                      (shelleyBasedEraConstraints sbe $ AnyShelleyBasedEra sbe)
                  )
                  $ scriptLanguageSupportedInEra sbe
                  $ PlutusScriptLanguage lang
              return
                ( stakeAddr
                , withdrawalAmt
                , Just $
                    WithdrawalScriptWitness $
                      PlutusScriptWitness
                        sLangSupported
                        lang
                        pScript
                        NoScriptDatumForStake
                        redeemer
                        execUnits
                )
        SimpleReferenceScript (SimpleRefScriptArgs refTxIn NoPolicyId) ->
          return
            ( stakeAddr
            , withdrawalAmt
            , Just $
                WithdrawalScriptWitness $
                  SimpleScriptWitness
                    (sbeToSimpleScriptLanguageInEra sbe)
                    (SReferenceScript refTxIn)
            )
        PlutusReferenceScript
          ( PlutusRefScriptCliArgs
              refTxIn
              anyPlutusScriptVersion
              NoScriptDatumAllowed
              NoPolicyId
              redeemerFile
              execUnits
            ) -> do
            case anyPlutusScriptVersion of
              AnyPlutusScriptVersion lang -> do
                let pScript = PReferenceScript refTxIn
                redeemer <-
                  fromExceptTCli $
                    readScriptDataOrFile redeemerFile
                sLangSupported <-
                  fromMaybeCli
                    ( PlutusScriptWitnessLanguageNotSupportedInEra
                        (AnyPlutusScriptVersion lang)
                        (shelleyBasedEraConstraints sbe $ AnyShelleyBasedEra sbe)
                    )
                    $ scriptLanguageSupportedInEra sbe
                    $ PlutusScriptLanguage lang

                return
                  ( stakeAddr
                  , withdrawalAmt
                  , Just $
                      WithdrawalScriptWitness $
                        PlutusScriptWitness
                          sLangSupported
                          lang
                          pScript
                          NoScriptDatumForStake
                          redeemer
                          execUnits
                  )
