{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Script.Withdrawal.Read
  ( readWithdrawalScriptWitness
  )
where

import Cardano.Api
import Cardano.Api.Ledger
import Cardano.Api.Shelley

import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.EraBased.Script.Withdrawal.Type

readWithdrawalScriptWitness
  :: MonadIOTransError (FileError CliScriptWitnessError) t m
  => ShelleyBasedEra era
  -> (StakeAddress, Coin, Maybe CliWithdrawalScriptRequirements)
  -> t m (StakeAddress, Coin, Maybe (WithdrawalScriptWitness era))
readWithdrawalScriptWitness _ (stakeAddr, withdrawalAmt, Nothing) =
  return (stakeAddr, withdrawalAmt, Nothing)
readWithdrawalScriptWitness sbe (stakeAddr, withdrawalAmt, Just certScriptReq) = do
  case certScriptReq of
    OnDiskSimpleScript scriptFp -> do
      let sFp = unFile scriptFp
      s <-
        modifyError (fmap SimpleScriptWitnessDecodeError) $
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
    OnDiskPlutusScript (OnDiskPlutusScriptCliArgs scriptFp redeemerFile execUnits) -> do
      let plutusScriptFp = unFile scriptFp
      plutusScript <-
        modifyError (fmap PlutusScriptWitnessDecodeError) $
          readFilePlutusScript plutusScriptFp
      redeemer <-
        modifyError (FileError plutusScriptFp . PlutusScriptWitnessRedeemerError) $
          readScriptDataOrFile redeemerFile
      case plutusScript of
        AnyPlutusScript lang script -> do
          let pScript = PScript script
          sLangSupported <-
            modifyError (FileError plutusScriptFp)
              $ hoistMaybe
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
    OnDiskPlutusRefScript (PlutusRefScriptCliArgs refTxIn anyPlutusScriptVersion redeemerFile execUnits) -> do
      case anyPlutusScriptVersion of
        AnyPlutusScriptVersion lang -> do
          let pScript = PReferenceScript refTxIn
          redeemer <-
            -- TODO: Implement a new error type to capture this. FileError is not representative of cases
            -- where we do not have access to the script.
            modifyError
              ( FileError "Reference script filepath not available"
                  . PlutusScriptWitnessRedeemerError
              )
              $ readScriptDataOrFile redeemerFile
          sLangSupported <-
            -- TODO: Implement a new error type to capture this. FileError is not representative of cases
            -- where we do not have access to the script.
            modifyError (FileError "Reference script filepath not available")
              $ hoistMaybe
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
