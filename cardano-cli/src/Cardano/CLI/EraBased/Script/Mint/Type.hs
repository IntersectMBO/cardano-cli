{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Script.Mint.Type
  ( createSimpleOrPlutusScriptFromCliArgs
  , createPlutusReferenceScriptFromCliArgs
  , createSimpleReferenceScriptFromCliArgs
  , MintScriptWitnessWithPolicyId (..)
  )
where

import Cardano.Api
import Cardano.Api.Experimental
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Type.Common (AnySLanguage (..), ScriptDataOrFile)

-- We always need the policy id when constructing a transaction that mints.
-- In the case of reference scripts, the user currently must provide the policy id (script hash)
-- in order to correctly construct the transaction.
data MintScriptWitnessWithPolicyId era
  = MintScriptWitnessWithPolicyId
  { mswPolId :: PolicyId
  , mswScriptWitness :: ScriptWitness WitCtxMint era
  }
  deriving Show

createSimpleOrPlutusScriptFromCliArgs
  :: File ScriptInAnyLang In
  -> Maybe (ScriptDataOrFile, ExecutionUnits)
  -> ScriptRequirements MintItem
createSimpleOrPlutusScriptFromCliArgs scriptFp Nothing =
  OnDiskSimpleScript scriptFp
createSimpleOrPlutusScriptFromCliArgs scriptFp (Just (redeemerFile, execUnits)) =
  OnDiskPlutusScript $
    OnDiskPlutusScriptCliArgs scriptFp Exp.NoScriptDatumAllowed redeemerFile execUnits

createSimpleReferenceScriptFromCliArgs
  :: TxIn
  -> PolicyId
  -> ScriptRequirements MintItem
createSimpleReferenceScriptFromCliArgs txin polid =
  SimpleReferenceScript $ SimpleRefScriptArgs txin polid

createPlutusReferenceScriptFromCliArgs
  :: TxIn
  -> AnySLanguage
  -> ScriptDataOrFile
  -> ExecutionUnits
  -> PolicyId
  -> ScriptRequirements MintItem
createPlutusReferenceScriptFromCliArgs txin scriptVersion scriptData execUnits polid =
  PlutusReferenceScript $
    PlutusRefScriptCliArgs txin scriptVersion Exp.NoScriptDatumAllowed polid scriptData execUnits
