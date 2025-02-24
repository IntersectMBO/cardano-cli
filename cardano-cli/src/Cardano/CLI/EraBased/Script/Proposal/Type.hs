{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Script.Proposal.Type
  ( CliProposalScriptRequirements (..)
  , PlutusRefScriptCliArgs (..)
  , PlutusScriptCliArgs (..)
  , ProposalScriptWitness (..)
  , createSimpleOrPlutusScriptFromCliArgs
  , createPlutusReferenceScriptFromCliArgs
  )
where

import Cardano.Api

import Cardano.CLI.Type.Common (ScriptDataOrFile)

newtype ProposalScriptWitness era
  = ProposalScriptWitness {pswScriptWitness :: ScriptWitness WitCtxStake era}
  deriving Show

data CliProposalScriptRequirements
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
  -> CliProposalScriptRequirements
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
  -> CliProposalScriptRequirements
createPlutusReferenceScriptFromCliArgs txIn version redeemer execUnits =
  OnDiskPlutusRefScript $ PlutusRefScriptCliArgs txIn version redeemer execUnits
