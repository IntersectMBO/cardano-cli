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
import Cardano.CLI.EraBased.Script.Mint.Type (MintScriptWitnessWithPolicyId (..))
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type
  ( AnyPlutusScript (..)
  , CliScriptWitnessError (..)
  , OnDiskPlutusScriptCliArgs (..)
  , PlutusRefScriptCliArgs (..)
  , ScriptRequirements (..)
  , SimpleRefScriptCliArgs (..)
  )
import Cardano.CLI.Read

readMintScriptWitness
  :: Exp.IsEra era
  => ScriptRequirements Exp.MintItem -> CIO e (MintScriptWitnessWithPolicyId era)
readMintScriptWitness (OnDiskSimpleScript scriptFp) = do
  let sFp = unFile scriptFp
  s <-
    readFileSimpleScript sFp

  case s of
    SimpleScript ss -> do
      let polId = PolicyId $ hashScript s
      return $
        MintScriptWitnessWithPolicyId polId $
          SimpleScriptWitness (sbeToSimpleScriptLanguageInEra $ convert Exp.useEra) $
            SScript ss
readMintScriptWitness
  ( OnDiskPlutusScript
      (OnDiskPlutusScriptCliArgs scriptFp Exp.NoScriptDatumAllowed redeemerFile execUnits)
    ) = do
    let plutusScriptFp = unFile scriptFp
    plutusScript <-
      readFilePlutusScript plutusScriptFp

    redeemer <-
      fromExceptTCli $
        readScriptDataOrFile redeemerFile
    case plutusScript of
      AnyPlutusScript lang script -> do
        let pScript = PScript script
            sbe = convert Exp.useEra
            polId = scriptPolicyId $ PlutusScript lang script
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
readMintScriptWitness
  ( PlutusReferenceScript
      ( PlutusRefScriptCliArgs
          refTxIn
          anyPlutusScriptVersion
          Exp.NoScriptDatumAllowed
          polId
          redeemerFile
          execUnits
        )
    ) = do
    case anyPlutusScriptVersion of
      AnyPlutusScriptVersion lang -> do
        let pScript = PReferenceScript refTxIn
            sbe = convert Exp.useEra
        redeemer <-
          fromExceptTCli $ readScriptDataOrFile redeemerFile
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
readMintScriptWitness (SimpleReferenceScript (SimpleRefScriptArgs refTxIn polId)) =
  return $
    MintScriptWitnessWithPolicyId polId $
      SimpleScriptWitness
        (sbeToSimpleScriptLanguageInEra $ convert Exp.useEra)
        (SReferenceScript refTxIn)
