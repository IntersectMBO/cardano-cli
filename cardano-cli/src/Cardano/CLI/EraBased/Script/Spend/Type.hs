{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Script.Spend.Type
  ( PlutusRefScriptCliArgs (..)
  , SimpleRefScriptCliArgs (..)
  , createSimpleOrPlutusScriptFromCliArgs
  , createPlutusReferenceScriptFromCliArgs
  , createSimpleReferenceScriptFromCliArgs
  )
where

import Cardano.Api
import Cardano.Api.Experimental

import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Type.Common (AnySLanguage, ScriptDataOrFile)

createSimpleOrPlutusScriptFromCliArgs
  :: File ScriptInAnyLang In
  -> Maybe (ScriptDatumOrFileSpending, ScriptDataOrFile, ExecutionUnits)
  -> ScriptRequirements TxInItem
createSimpleOrPlutusScriptFromCliArgs scriptFp (Just (datumFile, redeemerFile, execUnits)) =
  OnDiskPlutusScript $ OnDiskPlutusScriptCliArgs scriptFp datumFile redeemerFile execUnits
createSimpleOrPlutusScriptFromCliArgs scriptFp Nothing = OnDiskSimpleScript scriptFp

createSimpleReferenceScriptFromCliArgs :: TxIn -> ScriptRequirements TxInItem
createSimpleReferenceScriptFromCliArgs = SimpleReferenceScript . flip SimpleRefScriptArgs NoPolicyId

createPlutusReferenceScriptFromCliArgs
  :: TxIn
  -> AnySLanguage
  -> ScriptDatumOrFileSpending
  -> ScriptDataOrFile
  -> ExecutionUnits
  -> ScriptRequirements TxInItem
createPlutusReferenceScriptFromCliArgs txin v mDatum redeemer execUnits =
  PlutusReferenceScript $ PlutusRefScriptCliArgs txin v mDatum NoPolicyId redeemer execUnits
