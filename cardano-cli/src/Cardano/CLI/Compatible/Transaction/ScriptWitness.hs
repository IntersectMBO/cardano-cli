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
  ( AnyShelleyBasedEra (..)
  , File (..)
  , IsPlutusScriptLanguage
  , PlutusScriptOrReferenceInput (..)
  , PlutusScriptVersion (..)
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
import Cardano.Api.Experimental.Plutus (fromPlutusSLanguage)
import Cardano.Api.Experimental.Plutus qualified as Exp

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Compatible.Read
import Cardano.CLI.EraBased.Script.Certificate.Type
import Cardano.CLI.EraBased.Script.Read.Common (readScriptDataOrFile)
import Cardano.CLI.EraBased.Script.Type
  ( CliScriptWitnessError (..)
  , NoPolicyId (..)
  , OnDiskPlutusScriptCliArgs (..)
  , ScriptRequirements (..)
  , SimpleRefScriptCliArgs (..)
  )
import Cardano.CLI.EraBased.Script.Type qualified as Exp
import Cardano.CLI.Type.Common (AnySLanguage (..), CertificateFile)
import Cardano.Ledger.Plutus.Language qualified as L

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
        plutusScript <- readFilePlutusScript plutusScriptFp
        redeemer <-
          fromExceptTCli $
            readScriptDataOrFile redeemerFile
        case plutusScript of
          AnyPlutusScript lang script -> do
            let pScript = PScript script
            sLangSupported <-
              fromMaybeCli
                ( PlutusScriptWitnessLanguageNotSupportedInEra
                    (fromOldScriptLanguage lang)
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
          (AnySLanguage lang)
          Exp.NoScriptDatumAllowed
          Exp.NoPolicyId
          redeemerFile
          execUnits
        ) -> do
        let pScript = PReferenceScript refTxIn
        redeemer <-
          fromExceptTCli $
            readScriptDataOrFile redeemerFile
        sLangSupported <-
          fromMaybeCli
            ( PlutusScriptWitnessLanguageNotSupportedInEra
                (L.plutusLanguage lang)
                (shelleyBasedEraConstraints sbe $ AnyShelleyBasedEra sbe)
            )
            $ scriptLanguageSupportedInEra sbe
            $ obtainIsPlutusScriptLanguage (fromPlutusSLanguage lang)
            $ PlutusScriptLanguage
            $ Exp.fromPlutusSLanguage lang

        return $
          CertificateScriptWitness $
            obtainIsPlutusScriptLanguage (fromPlutusSLanguage lang) $
              PlutusScriptWitness
                sLangSupported
                (Exp.fromPlutusSLanguage lang)
                pScript
                NoScriptDatumForStake
                redeemer
                execUnits

fromOldScriptLanguage :: PlutusScriptVersion lang -> L.Language
fromOldScriptLanguage PlutusScriptV1 = L.PlutusV1
fromOldScriptLanguage PlutusScriptV2 = L.PlutusV2
fromOldScriptLanguage PlutusScriptV3 = L.PlutusV3
fromOldScriptLanguage PlutusScriptV4 = L.PlutusV4

obtainIsPlutusScriptLanguage
  :: PlutusScriptVersion lang
  -> (IsPlutusScriptLanguage lang => a)
  -> a
obtainIsPlutusScriptLanguage lang f =
  case lang of
    PlutusScriptV1 -> f
    PlutusScriptV2 -> f
    PlutusScriptV3 -> f
    PlutusScriptV4 -> f
