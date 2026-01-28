{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Script.Vote.Type
  ( createSimpleOrPlutusScriptFromCliArgs
  , createPlutusReferenceScriptFromCliArgs
  )
where

import Cardano.Api
  ( ExecutionUnits
  , File
  , FileDirection (In)
  , ScriptInAnyLang
  , TxIn
  )
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.EraBased.Script.Type qualified as Latest
import Cardano.CLI.Type.Common (AnySLanguage, ScriptDataOrFile)

createSimpleOrPlutusScriptFromCliArgs
  :: File ScriptInAnyLang In
  -> Maybe (ScriptDataOrFile, ExecutionUnits)
  -> Latest.ScriptRequirements Exp.VoterItem
createSimpleOrPlutusScriptFromCliArgs scriptFp (Just (redeemer, execUnits)) =
  Latest.OnDiskPlutusScript $
    Latest.OnDiskPlutusScriptCliArgs scriptFp Exp.NoScriptDatumAllowed redeemer execUnits
createSimpleOrPlutusScriptFromCliArgs scriptFp Nothing =
  Latest.OnDiskSimpleScript scriptFp

createPlutusReferenceScriptFromCliArgs
  :: TxIn
  -> AnySLanguage
  -> ScriptDataOrFile
  -> ExecutionUnits
  -> Latest.ScriptRequirements Exp.VoterItem
createPlutusReferenceScriptFromCliArgs txIn anySLang redeemer execUnits =
  Latest.PlutusReferenceScript $
    Latest.PlutusRefScriptCliArgs
      txIn
      anySLang
      Exp.NoScriptDatumAllowed
      Latest.NoPolicyId
      redeemer
      execUnits
