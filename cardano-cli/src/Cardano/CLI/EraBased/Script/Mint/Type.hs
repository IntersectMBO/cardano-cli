{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Script.Mint.Type
  ( CliMintScriptRequirements (..)
  , SimpleOrPlutusScriptCliArgs (..)
  , createSimpleOrPlutusScriptFromCliArgs
  , PlutusRefScriptCliArgs (..)
  , createPlutusReferenceScriptFromCliArgs
  , SimpleRefScriptCliArgs (..)
  , createSimpleReferenceScriptFromCliArgs
  , MintScriptWitnessWithPolicyId (..)
  )
where

import Cardano.Api

import Cardano.CLI.Type.Common (ScriptDataOrFile)

-- We always need the policy id when constructing a transaction that mints.
-- In the case of reference scripts, the user currently must provide the policy id (script hash)
-- in order to correctly construct the transaction.
data MintScriptWitnessWithPolicyId era
  = MintScriptWitnessWithPolicyId
  { mswPolId :: PolicyId
  , mswScriptWitness :: ScriptWitness WitCtxMint era
  }
  deriving Show

data CliMintScriptRequirements
  = OnDiskSimpleOrPlutusScript SimpleOrPlutusScriptCliArgs
  | OnDiskSimpleRefScript SimpleRefScriptCliArgs
  | OnDiskPlutusRefScript PlutusRefScriptCliArgs
  deriving Show

data SimpleOrPlutusScriptCliArgs
  = OnDiskSimpleScriptCliArgs
      (File ScriptInAnyLang In)
  | OnDiskPlutusScriptCliArgs
      (File ScriptInAnyLang In)
      ScriptDataOrFile
      -- ^ Redeemer
      ExecutionUnits
  deriving Show

createSimpleOrPlutusScriptFromCliArgs
  :: File ScriptInAnyLang In
  -> Maybe (ScriptDataOrFile, ExecutionUnits)
  -> CliMintScriptRequirements
createSimpleOrPlutusScriptFromCliArgs scriptFp Nothing =
  OnDiskSimpleOrPlutusScript $ OnDiskSimpleScriptCliArgs scriptFp
createSimpleOrPlutusScriptFromCliArgs scriptFp (Just (redeemerFile, execUnits)) =
  OnDiskSimpleOrPlutusScript $ OnDiskPlutusScriptCliArgs scriptFp redeemerFile execUnits

data SimpleRefScriptCliArgs
  = SimpleRefScriptCliArgs
      TxIn
      PolicyId
  deriving Show

createSimpleReferenceScriptFromCliArgs
  :: TxIn
  -> PolicyId
  -> CliMintScriptRequirements
createSimpleReferenceScriptFromCliArgs txin polid =
  OnDiskSimpleRefScript $ SimpleRefScriptCliArgs txin polid

data PlutusRefScriptCliArgs
  = PlutusRefScriptCliArgs
      TxIn
      AnyPlutusScriptVersion
      ScriptDataOrFile
      ExecutionUnits
      PolicyId
  deriving Show

createPlutusReferenceScriptFromCliArgs
  :: TxIn
  -> AnyPlutusScriptVersion
  -> ScriptDataOrFile
  -> ExecutionUnits
  -> PolicyId
  -> CliMintScriptRequirements
createPlutusReferenceScriptFromCliArgs txin scriptVersion scriptData execUnits polid =
  OnDiskPlutusRefScript $ PlutusRefScriptCliArgs txin scriptVersion scriptData execUnits polid
