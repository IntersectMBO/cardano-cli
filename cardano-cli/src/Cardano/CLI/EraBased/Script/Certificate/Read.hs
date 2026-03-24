{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Cardano.Api.Experimental.AnyScriptWitness
import Cardano.Api.Experimental.Plutus qualified as Exp.Plutus

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Orphan ()
import Cardano.CLI.Read
import Cardano.CLI.Type.Common (AnySLanguage (..), CertificateFile)

readCertificateScriptWitness
  :: forall era e
   . IsEra era
  => AnyNonAssetScript
  -> CIO e (AnyWitness (LedgerEra era))
readCertificateScriptWitness (AnyNonAssetScriptSimple simpleReq) =
  case simpleReq of
    OnDiskSimpleScript scriptFp -> do
      let sFp = unFile scriptFp
      AnySimpleScriptWitness . SScript <$> readFileSimpleScript sFp useEra
    ReferenceSimpleScript refTxin ->
      return . AnySimpleScriptWitness $ SReferenceScript refTxin
readCertificateScriptWitness (AnyNonAssetScriptPlutus plutusReq) =
  case plutusReq of
    OnDiskPlutusNonAssetScript scriptFp redeemerFile execUnits -> do
      let plutusScriptFp = unFile scriptFp
      Exp.Plutus.AnyPlutusScript script <-
        readFilePlutusScript @_ @era plutusScriptFp

      let
        lang = Exp.Plutus.plutusScriptInEraSLanguage script
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
    ReferencePlutusNonAssetScript refInput (AnySLanguage lang) redeemerFile execUnits -> do
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

readCertificateScriptWitnesses
  :: IsEra era
  => [(CertificateFile, Maybe AnyNonAssetScript)]
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
