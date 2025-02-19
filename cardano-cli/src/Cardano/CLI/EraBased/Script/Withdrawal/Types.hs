{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Script.Withdrawal.Types
  ( CliWithdrawalScriptRequirements (..)
  , PlutusRefScriptCliArgs (..)
  , PlutusScriptCliArgs (..)
  , WithdrawalScriptWitness (..)
  , createSimpleOrPlutusScriptFromCliArgs
  , createPlutusReferenceScriptFromCliArgs
  )
where

import Cardano.Api

import Cardano.CLI.Types.Common (ScriptDataOrFile)

newtype WithdrawalScriptWitness era
  = WithdrawalScriptWitness {wswScriptWitness :: ScriptWitness WitCtxStake era}
  deriving Show

data CliWithdrawalScriptRequirements
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
  -> CliWithdrawalScriptRequirements
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
  -> CliWithdrawalScriptRequirements
createPlutusReferenceScriptFromCliArgs txIn version redeemer execUnits =
  OnDiskPlutusRefScript $ PlutusRefScriptCliArgs txIn version redeemer execUnits
