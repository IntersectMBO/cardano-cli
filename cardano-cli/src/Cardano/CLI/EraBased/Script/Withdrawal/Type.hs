{-# LANGUAGE DataKinds #-}

module Cardano.CLI.EraBased.Script.Withdrawal.Type
  ( createSimpleOrPlutusScriptFromCliArgs
  , createPlutusReferenceScriptFromCliArgs
  )
where

import Cardano.Api

import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Type.Common (AnySLanguage (..), ScriptDataOrFile)

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
createPlutusReferenceScriptFromCliArgs txIn version redeemer execUnits =
  AnyNonAssetScriptPlutus $ ReferencePlutusNonAssetScript txIn version redeemer execUnits
