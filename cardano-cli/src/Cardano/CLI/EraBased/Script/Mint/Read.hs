{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.EraBased.Script.Mint.Read
  ( readMintScriptWitness
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Script.Mint.Type
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type (AnyPlutusScript (..), CliScriptWitnessError (..))
import Cardano.CLI.Type.Error.PlutusScriptDecodeError
import Cardano.CLI.Type.Error.ScriptDecodeError

readMintScriptWitness
  :: Exp.IsEra era
  => CliMintScriptRequirements -> CIO e (MintScriptWitnessWithPolicyId era)
readMintScriptWitness (OnDiskSimpleOrPlutusScript simpleOrPlutus) =
  case simpleOrPlutus of
    OnDiskSimpleScriptCliArgs simpleFp -> do
      let sFp = unFile simpleFp
      s <-
        fromExceptTCli
          ( readFileSimpleScript sFp
              :: (ExceptT (FileError ScriptDecodeError) IO (Script SimpleScript'))
          )
      case s of
        SimpleScript ss -> do
          let polId = PolicyId $ hashScript s
          return $
            MintScriptWitnessWithPolicyId polId $
              SimpleScriptWitness (sbeToSimpleScriptLanguageInEra $ convert Exp.useEra) $
                SScript ss
    OnDiskPlutusScriptCliArgs plutusScriptFp redeemerFile execUnits -> do
      let sFp = unFile plutusScriptFp
      plutusScript <-
        fromExceptTCli
          ( readFilePlutusScript sFp
              :: ExceptT (FileError PlutusScriptDecodeError) IO AnyPlutusScript
          )

      redeemer <-
        fromExceptTCli $
          readScriptDataOrFile redeemerFile
      case plutusScript of
        AnyPlutusScript lang script -> do
          let pScript = PScript script
              sbe = convert Exp.useEra
              polId = PolicyId $ hashScript $ PlutusScript lang script
          sLangSupported <-
            fromMaybeCli
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
readMintScriptWitness (OnDiskSimpleRefScript (SimpleRefScriptCliArgs refTxIn polId)) =
  return $
    MintScriptWitnessWithPolicyId polId $
      SimpleScriptWitness
        (sbeToSimpleScriptLanguageInEra $ convert Exp.useEra)
        (SReferenceScript refTxIn)
readMintScriptWitness
  ( OnDiskPlutusRefScript
      (PlutusRefScriptCliArgs refTxIn anyPlutusScriptVersion redeemerFile execUnits polId)
    ) = do
    case anyPlutusScriptVersion of
      AnyPlutusScriptVersion lang -> do
        let pScript = PReferenceScript refTxIn
            sbe = convert Exp.useEra
        redeemer <-
          -- TODO: Implement a new error type to capture this. FileError is not representative of cases
          -- where we do not have access to the script.
          fromExceptTCli $
            readScriptDataOrFile redeemerFile
        sLangSupported <-
          -- TODO: Implement a new error type to capture this. FileError is not representative of cases
          -- where we do not have access to the script.
          fromMaybeCli
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
