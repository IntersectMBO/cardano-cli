{-# LANGUAGE DataKinds #-}

module Cardano.CLI.EraBased.Script.Certificate.Type
  ( CertificateScriptWitness (..)
  , createSimpleOrPlutusScriptFromCliArgs
  , createPlutusReferenceScriptFromCliArgs
  )
where

import Cardano.Api

import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Type.Common (AnySLanguage (..), ScriptDataOrFile)

newtype CertificateScriptWitness era
  = CertificateScriptWitness {cswScriptWitness :: ScriptWitness WitCtxStake era}
  deriving Show

createSimpleOrPlutusScriptFromCliArgs
  :: File ScriptInAnyLang In
  -> Maybe (ScriptDataOrFile, ExecutionUnits)
  -> AnyNonAssetScript
createSimpleOrPlutusScriptFromCliArgs scriptFp (Just (redeemer, execUnits)) =
  AnyNonAssetScriptPlutus $ OnDiskPlutusNonAssetScript scriptFp redeemer execUnits
createSimpleOrPlutusScriptFromCliArgs scriptFp Nothing =
  AnyNonAssetScriptSimple $ OnDiskSimpleScript scriptFp

createPlutusReferenceScriptFromCliArgs
  :: TxIn
  -> AnySLanguage
  -> ScriptDataOrFile
  -> ExecutionUnits
  -> AnyNonAssetScript
createPlutusReferenceScriptFromCliArgs txIn l redeemer execUnits =
  AnyNonAssetScriptPlutus $ ReferencePlutusNonAssetScript txIn l redeemer execUnits
