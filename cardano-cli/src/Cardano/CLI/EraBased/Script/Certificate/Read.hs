{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Cardano.CLI.EraBased.Script.Certificate.Read
  ( readCertificateScriptWitness
  , readCertificateScriptWitnesses
  )
where

import Cardano.Api
import Cardano.Api.Shelley

import Cardano.CLI.EraBased.Script.Certificate.Type
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Type.Common (CertificateFile)

import Control.Monad

readCertificateScriptWitnesses
  :: MonadIOTransError (FileError CliScriptWitnessError) t m
  => ShelleyBasedEra era
  -> [(CertificateFile, Maybe CliCertificateScriptRequirements)]
  -> t m [(CertificateFile, Maybe (CertificateScriptWitness era))]
readCertificateScriptWitnesses sbe =
  mapM
    ( \(certFile, mSWit) -> do
        (certFile,) <$> forM mSWit (readCertificateScriptWitness sbe)
    )

readCertificateScriptWitness
  :: MonadIOTransError (FileError CliScriptWitnessError) t m
  => ShelleyBasedEra era -> CliCertificateScriptRequirements -> t m (CertificateScriptWitness era)
readCertificateScriptWitness sbe certScriptReq =
  case certScriptReq of
    OnDiskSimpleScript scriptFp -> do
      let sFp = unFile scriptFp
      s <-
        modifyError (fmap SimpleScriptWitnessDecodeError) $
          readFileSimpleScript sFp
      case s of
        SimpleScript ss -> do
          return $
            CertificateScriptWitness $
              SimpleScriptWitness (sbeToSimpleScriptLanguageInEra sbe) $
                SScript ss
    OnDiskPlutusScript (OnDiskPlutusScriptCliArgs scriptFp redeemerFile execUnits) -> do
      let plutusScriptFp = unFile scriptFp
      plutusScript <-
        modifyError (fmap PlutusScriptWitnessDecodeError) $
          readFilePlutusScript plutusScriptFp
      redeemer <-
        modifyError (FileError plutusScriptFp . PlutusScriptWitnessRedeemerError) $
          readScriptDataOrFile redeemerFile
      case plutusScript of
        AnyPlutusScript lang script -> do
          let pScript = PScript script
          sLangSupported <-
            modifyError (FileError plutusScriptFp)
              $ hoistMaybe
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
    OnDiskPlutusRefScript (PlutusRefScriptCliArgs refTxIn anyPlutusScriptVersion redeemerFile execUnits) -> do
      case anyPlutusScriptVersion of
        AnyPlutusScriptVersion lang -> do
          let pScript = PReferenceScript refTxIn
          redeemer <-
            -- TODO: Implement a new error type to capture this. FileError is not representative of cases
            -- where we do not have access to the script.
            modifyError
              ( FileError "Reference script filepath not available"
                  . PlutusScriptWitnessRedeemerError
              )
              $ readScriptDataOrFile redeemerFile
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
            CertificateScriptWitness $
              PlutusScriptWitness
                sLangSupported
                lang
                pScript
                NoScriptDatumForStake
                redeemer
                execUnits
