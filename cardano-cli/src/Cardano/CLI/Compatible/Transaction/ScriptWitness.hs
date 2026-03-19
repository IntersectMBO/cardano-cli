{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Cardano.CLI.Compatible.Transaction.ScriptWitness
  ( readCertificateScriptWitness
  , readCertificateScriptWitnesses
  )
where

import Cardano.Api
  ( DecoderError
  , SerialiseAsCBOR (serialiseToCBOR)
  , ShelleyBasedEra
  , ShelleyLedgerEra
  , shelleyBasedEraConstraints
  , unFile
  )
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.AnyScriptWitness qualified as Exp
import Cardano.Api.Experimental.Plutus qualified as Exp.Plutus

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Compatible.Read (readFilePlutusScript, readFileSimpleScript)
import Cardano.CLI.EraBased.Script.Read.Common (readScriptDataOrFile)
import Cardano.CLI.EraBased.Script.Type
  ( NoPolicyId (..)
  , OnDiskPlutusScriptCliArgs (..)
  , PlutusRefScriptCliArgs (..)
  , ScriptRequirements (..)
  , SimpleRefScriptCliArgs (..)
  )
import Cardano.CLI.Type.Common (AnySLanguage (..), CertificateFile)

import Control.Monad

readCertificateScriptWitnesses
  :: forall era e
   . ShelleyBasedEra era
  -> [(CertificateFile, Maybe (ScriptRequirements Exp.CertItem))]
  -> CIO e [(CertificateFile, Maybe (Exp.AnyWitness (ShelleyLedgerEra era)))]
readCertificateScriptWitnesses sbe =
  mapM
    ( \(certFile, mSWit) -> do
        (certFile,) <$> forM mSWit (readCertificateScriptWitness sbe)
    )

readCertificateScriptWitness
  :: forall era e
   . ShelleyBasedEra era
  -> ScriptRequirements Exp.CertItem
  -> CIO e (Exp.AnyWitness (ShelleyLedgerEra era))
readCertificateScriptWitness sbe certScriptReq =
  case certScriptReq of
    OnDiskSimpleScript scriptFp -> do
      let sFp = unFile scriptFp
      ss <- readFileSimpleScript sFp
      let serialisedSS = serialiseToCBOR ss
      simpleScript <-
        fromEitherCli
          ( shelleyBasedEraConstraints sbe (Exp.deserialiseSimpleScript serialisedSS)
              :: Either DecoderError (Exp.SimpleScript (ShelleyLedgerEra era))
          )
      return $ Exp.AnySimpleScriptWitness $ Exp.SScript simpleScript
    OnDiskPlutusScript
      (OnDiskPlutusScriptCliArgs scriptFp Exp.NoScriptDatumAllowed redeemerFile execUnits) -> do
        let plutusScriptFp = unFile scriptFp
        Exp.Plutus.AnyPlutusScript anyPlutusScript <- readFilePlutusScript sbe plutusScriptFp
        let lang = Exp.Plutus.plutusScriptInEraSLanguage anyPlutusScript
            script' = Exp.PScript anyPlutusScript
        redeemer <-
          fromExceptTCli $
            readScriptDataOrFile redeemerFile
        let sw =
              Exp.PlutusScriptWitness
                lang
                script'
                Exp.NoScriptDatum
                redeemer
                execUnits
        return $ Exp.AnyPlutusScriptWitness $ Exp.AnyPlutusCertifyingScriptWitness sw
    SimpleReferenceScript (SimpleRefScriptArgs refTxIn NoPolicyId) ->
      return . Exp.AnySimpleScriptWitness $ Exp.SReferenceScript refTxIn
    PlutusReferenceScript
      ( PlutusRefScriptCliArgs
          refInput
          (AnySLanguage lang)
          Exp.NoScriptDatumAllowed
          NoPolicyId
          redeemerFile
          execUnits
        ) -> do
        redeemer <-
          fromExceptTCli $
            readScriptDataOrFile redeemerFile
        return $
          Exp.AnyPlutusScriptWitness $
            Exp.AnyPlutusCertifyingScriptWitness $
              Exp.PlutusScriptWitness
                lang
                (Exp.PReferenceScript refInput)
                Exp.NoScriptDatum
                redeemer
                execUnits
