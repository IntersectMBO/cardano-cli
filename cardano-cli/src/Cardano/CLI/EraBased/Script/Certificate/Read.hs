{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Script.Certificate.Read
  ( readCertificateScriptWitness
  , readCertificateScriptWitnesses
  )
where

import Cardano.Api (File (..))
import Cardano.Api qualified as Api
import Cardano.Api.Experimental
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Plutus (AnyPlutusScriptVersion (..), ToLedgerPlutusLanguage)

import Cardano.CLI.Compatible.Exception
-- import Cardano.CLI.EraBased.Script.Certificate.Type
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Orphan ()
import Cardano.CLI.Read
import Cardano.CLI.Type.Common (CertificateFile)
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Plutus.Language qualified as L
import Cardano.Ledger.Plutus.Language qualified as Plutus

readCertificateScriptWitness
  :: forall era e
   . IsEra era
  => ScriptRequirements Exp.CertItem
  -> CIO e (AnyWitness (LedgerEra era))
readCertificateScriptWitness (OnDiskSimpleScript scriptFp) = do
  let sFp = unFile scriptFp
  s <-
    readFileSimpleScript sFp
  let nativeScript :: SimpleScript (LedgerEra era) = convertTotimelock useEra s
  return $
    AnySimpleScriptWitness $
      SScript nativeScript
readCertificateScriptWitness
  ( OnDiskPlutusScript
      (OnDiskPlutusScriptCliArgs scriptFp Exp.NoScriptDatumAllowed redeemerFile execUnits)
    ) = do
    let plutusScriptFp = unFile scriptFp
    AnyPlutusScript sVer apiScript <-
      readFilePlutusScript plutusScriptFp

    let lang = toPlutusSLanguage sVer
    script <- decodePlutusScript useEra sVer apiScript

    redeemer <-
      fromExceptTCli $
        readScriptDataOrFile redeemerFile
    return $
      AnyPlutusScriptWitness $
        PlutusScriptWitness
          lang
          script
          NoScriptDatum
          redeemer
          execUnits
readCertificateScriptWitness
  ( PlutusReferenceScript
      ( PlutusRefScriptCliArgs
          refInput
          (AnyPlutusScriptVersion sVer)
          Exp.NoScriptDatumAllowed
          NoPolicyId
          redeemerFile
          execUnits
        )
    ) = do
    let lang = toPlutusSLanguage sVer
    redeemer <-
      fromExceptTCli $
        readScriptDataOrFile redeemerFile
    return $
      AnyPlutusScriptWitness $
        PlutusScriptWitness
          lang
          (PReferenceScript refInput)
          NoScriptDatum
          redeemer
          execUnits
readCertificateScriptWitness (SimpleReferenceScript (SimpleRefScriptArgs refTxin NoPolicyId)) =
  return . AnySimpleScriptWitness $ SReferenceScript refTxin

decodePlutusScript
  :: forall era lang e
   . Era era
  -> Api.PlutusScriptVersion lang
  -> Api.PlutusScript lang
  -> CIO e (PlutusScriptOrReferenceInput (ToLedgerPlutusLanguage lang) (LedgerEra era))
decodePlutusScript era sVer (Api.PlutusScriptSerialised script) = obtainConstraints sVer $ do
  let runnableScriptBs = L.Plutus $ L.PlutusBinary script
  plutusRunnable <-
    fromEitherCli $
      Plutus.decodePlutusRunnable
        (getVersion era)
        runnableScriptBs
  return $ PScript (PlutusScriptInEra plutusRunnable)

obtainConstraints
  :: Api.PlutusScriptVersion lang
  -> (L.PlutusLanguage (ToLedgerPlutusLanguage lang) => a)
  -> a
obtainConstraints v =
  case v of
    Api.PlutusScriptV1 -> id
    Api.PlutusScriptV2 -> id
    Api.PlutusScriptV3 -> id
    Api.PlutusScriptV4 -> id

getVersion :: forall era. Era era -> L.Version
getVersion e = obtainCommonConstraints e $ L.eraProtVerLow @(LedgerEra era)

convertTotimelock
  :: forall era
   . Era era
  -> Api.Script Api.SimpleScript'
  -> SimpleScript (LedgerEra era)
convertTotimelock era (Api.SimpleScript s) =
  let native :: L.NativeScript (LedgerEra era) = obtainCommonConstraints era $ Api.toAllegraTimelock s
   in obtainCommonConstraints era $ SimpleScript native

readCertificateScriptWitnesses
  :: IsEra era
  => [(CertificateFile, Maybe (ScriptRequirements Exp.CertItem))]
  -> CIO e [(CertificateFile, AnyWitness (LedgerEra era))]
readCertificateScriptWitnesses certs =
  mapM
    ( \(vFile, mCert) -> do
        case mCert of
          Nothing -> return (vFile, AnyKeyWitnessPlaceholder)
          Just cert -> do
            sWit <- readCertificateScriptWitness cert
            return (vFile, sWit)
    )
    certs
