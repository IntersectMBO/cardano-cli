{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Script.Certificate.Type
  ( CertificateScriptWitness (..)
  , CliCertificateScriptRequirements (..)
  , PlutusScriptCliArgs (..)
  , PlutusRefScriptCliArgs (..)
  , createSimpleOrPlutusScriptFromCliArgs
  , createPlutusReferenceScriptFromCliArgs
  )
where

import Cardano.Api

import Cardano.CLI.Type.Common (ScriptDataOrFile)

newtype CertificateScriptWitness era
  = CertificateScriptWitness {cswScriptWitness :: ScriptWitness WitCtxStake era}
  deriving Show

data CliCertificateScriptRequirements
  = OnDiskPlutusScript PlutusScriptCliArgs
  | OnDiskSimpleScript (File ScriptInAnyLang In)
  | OnDiskPlutusRefScript PlutusRefScriptCliArgs
  deriving Show

data PlutusScriptCliArgs
  = OnDiskPlutusScriptCliArgs
      (File ScriptInAnyLang In)
      ScriptDataOrFile
      -- ^ Redeemer
      ExecutionUnits
  deriving Show

createSimpleOrPlutusScriptFromCliArgs
  :: File ScriptInAnyLang In
  -> Maybe (ScriptDataOrFile, ExecutionUnits)
  -> CliCertificateScriptRequirements
createSimpleOrPlutusScriptFromCliArgs scriptFp (Just (redeemer, execUnits)) =
  OnDiskPlutusScript $ OnDiskPlutusScriptCliArgs scriptFp redeemer execUnits
createSimpleOrPlutusScriptFromCliArgs scriptFp Nothing =
  OnDiskSimpleScript scriptFp

data PlutusRefScriptCliArgs
  = PlutusRefScriptCliArgs
      TxIn
      -- ^ TxIn with reference script
      AnyPlutusScriptVersion
      ScriptDataOrFile
      -- ^ Redeemer
      ExecutionUnits
  deriving Show

createPlutusReferenceScriptFromCliArgs
  :: TxIn
  -> AnyPlutusScriptVersion
  -> ScriptDataOrFile
  -> ExecutionUnits
  -> CliCertificateScriptRequirements
createPlutusReferenceScriptFromCliArgs txIn version redeemer execUnits =
  OnDiskPlutusRefScript $ PlutusRefScriptCliArgs txIn version redeemer execUnits
