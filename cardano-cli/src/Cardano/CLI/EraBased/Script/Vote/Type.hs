{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Script.Vote.Type
  ( VoteScriptWitness (..)
  , createSimpleOrPlutusScriptFromCliArgs
  , createPlutusReferenceScriptFromCliArgs
  )
where

import Cardano.Api
  ( AnyPlutusScriptVersion
  , ExecutionUnits
  , File
  , FileDirection (In)
  , ScriptInAnyLang
  , ScriptWitness
  , TxIn
  , WitCtxStake
  )
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.EraBased.Script.Type qualified as Latest
import Cardano.CLI.Type.Common (ScriptDataOrFile)

newtype VoteScriptWitness era
  = VoteScriptWitness {vswScriptWitness :: ScriptWitness WitCtxStake era}
  deriving Show

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
  -> AnyPlutusScriptVersion
  -> ScriptDataOrFile
  -> ExecutionUnits
  -> Latest.ScriptRequirements Exp.VoterItem
createPlutusReferenceScriptFromCliArgs txIn version redeemer execUnits =
  Latest.PlutusReferenceScript $
    Latest.PlutusRefScriptCliArgs
      txIn
      version
      Exp.NoScriptDatumAllowed
      Latest.NoPolicyId
      redeemer
      execUnits
