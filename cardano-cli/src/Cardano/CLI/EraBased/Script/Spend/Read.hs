{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Script.Spend.Read
  ( CliSpendScriptWitnessError
  , readSpendScriptWitness
  )
where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Script.Spend.Types
import           Cardano.CLI.EraBased.Script.Mint.Types (CliScriptWitnessError(..))
import           Cardano.CLI.Read



data CliSpendScriptWitnessError 
    = CliScriptWitnessError CliScriptWitnessError
    | CliSpendScriptWitnessDatumError ScriptDataError


instance Error CliSpendScriptWitnessError where 
    prettyError = \case 
        CliScriptWitnessError e -> prettyError e
        CliSpendScriptWitnessDatumError e -> renderScriptDataError e

readSpendScriptWitness
  :: MonadIOTransError (FileError CliSpendScriptWitnessError) t m
  => ShelleyBasedEra era -> CliSpendScriptRequirements -> t m (SpendScriptWitness era)
readSpendScriptWitness sbe spendScriptReq = 
    case spendScriptReq of 
      OnDiskSimpleOrPlutusScript (OnDiskSimpleCliArgs simpleFp) -> do
        let sFp = unFile simpleFp
        s <-
          modifyError (fmap (CliScriptWitnessError . SimpleScriptWitnessDecodeError)) $ readFileSimpleScript sFp
        case s of
          SimpleScript ss -> do
            return $
              SpendScriptWitness $
                SimpleScriptWitness (sbeToSimpleScriptLanguageInEra sbe) $
                  SScript ss
      OnDiskSimpleOrPlutusScript (OnDiskPlutusScriptCliArgs plutusScriptFp mScriptDatum redeemerFile execUnits) -> do
        let sFp = unFile plutusScriptFp
        plutusScript <-
          modifyError (fmap (CliScriptWitnessError . PlutusScriptWitnessDecodeError)) $
            readFilePlutusScript $
              unFile plutusScriptFp
        redeemer <-
          modifyError (FileError sFp . (CliScriptWitnessError . PlutusScriptWitnessRedeemerError)) $
            readScriptDataOrFile redeemerFile
        case plutusScript of
          AnyPlutusScript lang script -> do
            let pScript = PScript script
            sLangSupported <-
              modifyError (FileError sFp)
                $ hoistMaybe
                  ( CliScriptWitnessError $ PlutusScriptWitnessLanguageNotSupportedInEra
                      (AnyPlutusScriptVersion lang)
                      (shelleyBasedEraConstraints sbe $ AnyShelleyBasedEra sbe)
                  )
                $ scriptLanguageSupportedInEra sbe
                $ PlutusScriptLanguage lang
            mDatum <- handlePotentialScriptDatum mScriptDatum
            return $
              SpendScriptWitness $
                PlutusScriptWitness
                  sLangSupported
                  lang
                  pScript
                  mDatum
                  redeemer
                  execUnits 
      OnDiskSimpleRefScript (SimpleRefScriptArgs refTxIn) -> 
        return $ SpendScriptWitness $  SimpleScriptWitness
           (sbeToSimpleScriptLanguageInEra sbe)
           (SReferenceScript refTxIn) 
      OnDiskPlutusRefScript (PlutusRefScriptCliArgs refTxIn anyPlutusScriptVersion mScriptDatum redeemerFile execUnits) -> 
         case anyPlutusScriptVersion of
           AnyPlutusScriptVersion lang -> do
             let pScript = PReferenceScript refTxIn
             redeemer <-
               -- TODO: Implement a new error type to capture this. FileError is not representative of cases
               -- where we do not have access to the script.
               modifyError (FileError "Reference script filepath not available" . CliScriptWitnessError . PlutusScriptWitnessRedeemerError) $
                 readScriptDataOrFile redeemerFile
             sLangSupported <-
               -- TODO: Implement a new error type to capture this. FileError is not representative of cases
               -- where we do not have access to the script.
               modifyError (FileError "Reference script filepath not available")
                 $ hoistMaybe
                   ( CliScriptWitnessError $ PlutusScriptWitnessLanguageNotSupportedInEra
                       (AnyPlutusScriptVersion lang)
                       (shelleyBasedEraConstraints sbe $ AnyShelleyBasedEra sbe)
                   )
                 $ scriptLanguageSupportedInEra sbe
                 $ PlutusScriptLanguage lang

             mDatum <- handlePotentialScriptDatum mScriptDatum
             return $
               SpendScriptWitness $
                 PlutusScriptWitness
                   sLangSupported
                   lang
                   pScript
                   mDatum
                   redeemer
                   execUnits 


handlePotentialScriptDatum 
  :: MonadIOTransError (FileError CliSpendScriptWitnessError) t m
  => ScriptDatumOrFileSpending 
  -> t m (ScriptDatum WitCtxTxIn)
handlePotentialScriptDatum InlineDatum = return $ InlineScriptDatum
handlePotentialScriptDatum (PotentialDatum mDatum) = 
  case mDatum of 
    Just datumFp -> do
          sDatum <- modifyError (FileError (show datumFp) . CliSpendScriptWitnessDatumError) $ readScriptDataOrFile datumFp 
          return . ScriptDatumForTxIn $ Just sDatum
    Nothing -> return $ ScriptDatumForTxIn Nothing