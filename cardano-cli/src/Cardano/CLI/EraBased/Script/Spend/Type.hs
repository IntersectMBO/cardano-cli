{-# LANGUAGE DataKinds #-}

module Cardano.CLI.EraBased.Script.Spend.Type
  ( createSimpleOrPlutusScriptFromCliArgs
  , createPlutusReferenceScriptFromCliArgs
  , createSimpleReferenceScriptFromCliArgs
  )
where

import Cardano.Api

import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Type.Common (AnySLanguage, ScriptDataOrFile)

createSimpleOrPlutusScriptFromCliArgs
  :: File ScriptInAnyLang In
  -> Maybe (ScriptDatumOrFileSpending, ScriptDataOrFile, ExecutionUnits)
  -> AnySpendScript
createSimpleOrPlutusScriptFromCliArgs scriptFp (Just (datumFile, redeemerFile, execUnits)) =
  AnySpendScriptPlutus $ OnDiskPlutusSpendingScript scriptFp datumFile redeemerFile execUnits
createSimpleOrPlutusScriptFromCliArgs scriptFp Nothing =
  AnySpendScriptSimple $ OnDiskSimpleScript scriptFp

createSimpleReferenceScriptFromCliArgs :: TxIn -> AnySpendScript
createSimpleReferenceScriptFromCliArgs txin =
  AnySpendScriptSimple $ ReferenceSimpleScript txin

createPlutusReferenceScriptFromCliArgs
  :: TxIn
  -> AnySLanguage
  -> ScriptDatumOrFileSpending
  -> ScriptDataOrFile
  -> ExecutionUnits
  -> AnySpendScript
createPlutusReferenceScriptFromCliArgs txin v mDatum redeemer execUnits =
  AnySpendScriptPlutus $ ReferencePlutusSpendingScript txin v mDatum redeemer execUnits
