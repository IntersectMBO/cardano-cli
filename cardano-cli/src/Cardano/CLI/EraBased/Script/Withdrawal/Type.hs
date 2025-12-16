{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Script.Withdrawal.Type
  ( PlutusRefScriptCliArgs (..)
  , createSimpleOrPlutusScriptFromCliArgs
  , createPlutusReferenceScriptFromCliArgs
  )
where

import Cardano.Api
import Cardano.Api.Experimental

import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Type.Common (AnySLanguage (..), ScriptDataOrFile)

createSimpleOrPlutusScriptFromCliArgs
  :: File ScriptInAnyLang In
  -> Maybe (ScriptDataOrFile, ExecutionUnits)
  -> ScriptRequirements WithdrawalItem
createSimpleOrPlutusScriptFromCliArgs scriptFp (Just (redeemer, execUnits)) =
  OnDiskPlutusScript $ OnDiskPlutusScriptCliArgs scriptFp NoScriptDatumAllowed redeemer execUnits
createSimpleOrPlutusScriptFromCliArgs scriptFp Nothing =
  OnDiskSimpleScript scriptFp

createPlutusReferenceScriptFromCliArgs
  :: TxIn
  -> AnySLanguage
  -> ScriptDataOrFile
  -> ExecutionUnits
  -> ScriptRequirements WithdrawalItem
createPlutusReferenceScriptFromCliArgs txIn version redeemer execUnits =
  PlutusReferenceScript $
    PlutusRefScriptCliArgs txIn version NoScriptDatumAllowed NoPolicyId redeemer execUnits
