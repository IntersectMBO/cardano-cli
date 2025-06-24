{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Script.Proposal.Type
  ( PlutusRefScriptCliArgs (..)
  , ProposalScriptWitness (..)
  , createSimpleOrPlutusScriptFromCliArgs
  , createPlutusReferenceScriptFromCliArgs
  )
where

import Cardano.Api
import Cardano.Api.Experimental
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Type.Common (ScriptDataOrFile)

newtype ProposalScriptWitness era
  = ProposalScriptWitness {pswScriptWitness :: ScriptWitness WitCtxStake era}
  deriving Show

createSimpleOrPlutusScriptFromCliArgs
  :: File ScriptInAnyLang In
  -> Maybe (ScriptDataOrFile, ExecutionUnits)
  -> ScriptRequirements ProposalItem
createSimpleOrPlutusScriptFromCliArgs scriptFp (Just (redeemer, execUnits)) =
  OnDiskPlutusScript $ OnDiskPlutusScriptCliArgs scriptFp Exp.NoScriptDatumAllowed redeemer execUnits
createSimpleOrPlutusScriptFromCliArgs scriptFp Nothing =
  OnDiskSimpleScript scriptFp

createPlutusReferenceScriptFromCliArgs
  :: TxIn
  -> AnyPlutusScriptVersion
  -> ScriptDataOrFile
  -> ExecutionUnits
  -> ScriptRequirements ProposalItem
createPlutusReferenceScriptFromCliArgs txIn version redeemer execUnits =
  PlutusReferenceScript $
    PlutusRefScriptCliArgs txIn version Exp.NoScriptDatumAllowed NoPolicyId redeemer execUnits
