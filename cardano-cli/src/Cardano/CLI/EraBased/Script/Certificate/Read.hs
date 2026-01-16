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
import Cardano.Api.Experimental
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.AnyScriptWitness
import Cardano.Api.Experimental.Plutus qualified as Exp

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Orphan ()
import Cardano.CLI.Read
import Cardano.CLI.Type.Common (AnySLanguage (..), CertificateFile)

readCertificateScriptWitness
  :: forall era e
   . IsEra era
  => ScriptRequirements Exp.CertItem
  -> CIO e (AnyWitness (LedgerEra era))
readCertificateScriptWitness (OnDiskSimpleScript scriptFp) = do
  let sFp = unFile scriptFp
  AnySimpleScriptWitness . SScript <$> readFileSimpleScript sFp useEra
readCertificateScriptWitness
  ( OnDiskPlutusScript
      (OnDiskPlutusScriptCliArgs scriptFp Exp.NoScriptDatumAllowed redeemerFile execUnits)
    ) = do
    let plutusScriptFp = unFile scriptFp
    Exp.AnyPlutusScript script <-
      readFilePlutusScript @_ @era plutusScriptFp

    let
      lang = Exp.plutusScriptInEraSLanguage script
      script' = PScript script

    redeemer <-
      fromExceptTCli $
        readScriptDataOrFile redeemerFile

    let sw =
          PlutusScriptWitness
            lang
            script'
            NoScriptDatum
            redeemer
            execUnits
    return $
      AnyPlutusScriptWitness $
        AnyPlutusCertifyingScriptWitness sw
readCertificateScriptWitness
  ( PlutusReferenceScript
      ( PlutusRefScriptCliArgs
          refInput
          (AnySLanguage lang)
          Exp.NoScriptDatumAllowed
          NoPolicyId
          redeemerFile
          execUnits
        )
    ) = do
    redeemer <-
      fromExceptTCli $
        readScriptDataOrFile redeemerFile
    return $
      AnyPlutusScriptWitness $
        AnyPlutusCertifyingScriptWitness $
          PlutusScriptWitness
            lang
            (PReferenceScript refInput)
            NoScriptDatum
            redeemer
            execUnits
readCertificateScriptWitness (SimpleReferenceScript (SimpleRefScriptArgs refTxin NoPolicyId)) =
  return . AnySimpleScriptWitness $ SReferenceScript refTxin

readCertificateScriptWitnesses
  :: IsEra era
  => [(CertificateFile, Maybe (ScriptRequirements Exp.CertItem))]
  -> CIO e [(CertificateFile, AnyWitness (LedgerEra era))]
readCertificateScriptWitnesses =
  mapM
    ( \(vFile, mCert) -> do
        case mCert of
          Nothing -> return (vFile, AnyKeyWitnessPlaceholder)
          Just cert -> do
            sWit <- readCertificateScriptWitness cert
            return (vFile, sWit)
    )
