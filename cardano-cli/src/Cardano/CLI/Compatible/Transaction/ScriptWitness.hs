{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Cardano.CLI.Compatible.Transaction.ScriptWitness
  ( readCertificateScriptWitness
  , readCertificateScriptWitnesses
  )
where

import Cardano.Api
  ( AnyPlutusScriptVersion (..)
  , AnyShelleyBasedEra (..)
  , File (..)
  , PlutusScriptOrReferenceInput (..)
  , Script (..)
  , ScriptDatum (..)
  , ScriptLanguage (..)
  , ScriptWitness (..)
  , ShelleyBasedEra
  , SimpleScriptOrReferenceInput (..)
  , sbeToSimpleScriptLanguageInEra
  , scriptLanguageSupportedInEra
  , shelleyBasedEraConstraints
  )
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Script.Certificate.Type
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.EraBased.Script.Type qualified as Exp
import Cardano.CLI.Read
import Cardano.CLI.Type.Common (CertificateFile)

import Control.Monad

readCertificateScriptWitnesses
  :: ShelleyBasedEra era
  -> [(CertificateFile, Maybe (ScriptRequirements Exp.CertItem))]
  -> CIO e [(CertificateFile, Maybe (CertificateScriptWitness era))]
readCertificateScriptWitnesses sbe =
  mapM
    ( \(certFile, mSWit) -> do
        (certFile,) <$> forM mSWit (readCertificateScriptWitness sbe)
    )

readCertificateScriptWitness
  :: ShelleyBasedEra era -> ScriptRequirements Exp.CertItem -> CIO e (CertificateScriptWitness era)
readCertificateScriptWitness sbe certScriptReq =
  case certScriptReq of
    OnDiskSimpleScript scriptFp -> do
      let sFp = unFile scriptFp
      s <-
        readFileSimpleScript sFp
      case s of
        SimpleScript ss -> do
          return $
            CertificateScriptWitness $
              SimpleScriptWitness (sbeToSimpleScriptLanguageInEra sbe) $
                SScript ss
    OnDiskPlutusScript
      (OnDiskPlutusScriptCliArgs scriptFp Exp.NoScriptDatumAllowed redeemerFile execUnits) -> do
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
            return $
              CertificateScriptWitness $
                PlutusScriptWitness
                  sLangSupported
                  lang
                  pScript
                  NoScriptDatumForStake
                  redeemer
                  execUnits
    SimpleReferenceScript (SimpleRefScriptArgs refTxIn NoPolicyId) ->
      return $
        CertificateScriptWitness $
          SimpleScriptWitness
            (sbeToSimpleScriptLanguageInEra sbe)
            (SReferenceScript refTxIn)
    PlutusReferenceScript
      ( PlutusRefScriptCliArgs
          refTxIn
          anyPlutusScriptVersion
          Exp.NoScriptDatumAllowed
          Exp.NoPolicyId
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

            return $
              CertificateScriptWitness $
                PlutusScriptWitness
                  sLangSupported
                  lang
                  pScript
                  NoScriptDatumForStake
                  redeemer
                  execUnits
