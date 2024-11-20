{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Script.Mint.Read
  ( readMintScriptWitness
  )
where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Script.Mint.Types
import           Cardano.CLI.Read

readMintScriptWitness
  :: MonadIOTransError (FileError CliScriptWitnessError) t m
  => ShelleyBasedEra era -> CliMintScriptRequirements -> t m (MintScriptWitnessWithPolicyId era)
readMintScriptWitness sbe (OnDiskSimpleOrPlutusScript simpleOrPlutus) =
  case simpleOrPlutus of
    OnDiskSimpleScriptCliArgs simpleFp -> do
      let sFp = unFile simpleFp
      s <-
        modifyError (fmap SimpleScriptWitnessDecodeError) $ readFileSimpleScript sFp
      case s of
        SimpleScript ss -> do
          let polId = PolicyId $ hashScript s
          return $
            MintScriptWitnessWithPolicyId polId $
              SimpleScriptWitness (sbeToSimpleScriptLangInEra sbe) $
                SScript ss
    OnDiskPlutusScriptCliArgs plutusScriptFp redeemerFile execUnits -> do
      let sFp = unFile plutusScriptFp
      plutusScript <-
        modifyError (fmap PlutusScriptWitnessDecodeError) $
          readFilePlutusScript $
            unFile plutusScriptFp

      redeemer <-
        modifyError (FileError sFp . PlutusScriptWitnessRedeemerError) $
          readScriptDataOrFile redeemerFile
      case plutusScript of
        AnyPlutusScript lang script -> do
          let pScript = PScript script
              polId = PolicyId $ hashScript $ PlutusScript lang script
          sLangSupported <-
            modifyError (FileError sFp)
              $ hoistMaybe
                ( PlutusScriptWitnessLanguageNotSupportedInEra
                    (AnyPlutusScriptVersion lang)
                    (shelleyBasedEraConstraints sbe $ AnyShelleyBasedEra sbe)
                )
              $ scriptLanguageSupportedInEra sbe
              $ PlutusScriptLanguage lang
          return $
            MintScriptWitnessWithPolicyId polId $
              PlutusScriptWitness
                sLangSupported
                lang
                pScript
                NoScriptDatumForMint
                redeemer
                execUnits
readMintScriptWitness sbe (OnDiskSimpleRefScript (SimpleRefScriptCliArgs refTxIn polId)) =
  return $
    MintScriptWitnessWithPolicyId polId $
      SimpleScriptWitness
        (sbeToSimpleScriptLangInEra sbe)
        (SReferenceScript refTxIn $ Just $ unPolicyId polId)
readMintScriptWitness
  sbe
  ( OnDiskPlutusRefScript
      (PlutusRefScriptCliArgs refTxIn anyPlutusScriptVersion redeemerFile execUnits polId)
    ) = do
    case anyPlutusScriptVersion of
      AnyPlutusScriptVersion lang -> do
        let pScript = PReferenceScript refTxIn $ Just $ unPolicyId polId
        redeemer <-
          -- TODO: Implement a new error type to capture this. FileError is not representative of cases
          -- where we do not have access to the script.
          modifyError (FileError "Reference script filepath not available" . PlutusScriptWitnessRedeemerError) $
            readScriptDataOrFile redeemerFile
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
        return $
          MintScriptWitnessWithPolicyId polId $
            PlutusScriptWitness
              sLangSupported
              lang
              pScript
              NoScriptDatumForMint
              redeemer
              execUnits

-- TODO: Remove me when exposed from cardano-api
sbeToSimpleScriptLangInEra
  :: ShelleyBasedEra era -> ScriptLanguageInEra SimpleScript' era
sbeToSimpleScriptLangInEra ShelleyBasedEraShelley = SimpleScriptInShelley
sbeToSimpleScriptLangInEra ShelleyBasedEraAllegra = SimpleScriptInAllegra
sbeToSimpleScriptLangInEra ShelleyBasedEraMary = SimpleScriptInMary
sbeToSimpleScriptLangInEra ShelleyBasedEraAlonzo = SimpleScriptInAlonzo
sbeToSimpleScriptLangInEra ShelleyBasedEraBabbage = SimpleScriptInBabbage
sbeToSimpleScriptLangInEra ShelleyBasedEraConway = SimpleScriptInConway
