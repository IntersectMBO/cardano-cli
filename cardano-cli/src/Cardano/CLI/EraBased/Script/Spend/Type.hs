{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Script.Spend.Type
  ( CliSpendScriptRequirements (..)
  , PlutusRefScriptCliArgs (..)
  , SimpleOrPlutusScriptCliArgs (..)
  , ScriptDatumOrFileSpending (..)
  , SimpleRefScriptCliArgs (..)
  , SpendScriptWitness (..)
  , createSimpleOrPlutusScriptFromCliArgs
  , createPlutusReferenceScriptFromCliArgs
  , createSimpleReferenceScriptFromCliArgs
  )
where

import Cardano.Api

import Cardano.CLI.Type.Common (ScriptDataOrFile)

newtype SpendScriptWitness era
  = SpendScriptWitness {sswScriptWitness :: ScriptWitness WitCtxTxIn era}
  deriving Show

data CliSpendScriptRequirements
  = OnDiskSimpleOrPlutusScript SimpleOrPlutusScriptCliArgs
  | OnDiskSimpleRefScript SimpleRefScriptCliArgs
  | OnDiskPlutusRefScript PlutusRefScriptCliArgs
  deriving Show

data SimpleOrPlutusScriptCliArgs
  = OnDiskPlutusScriptCliArgs
      (File ScriptInAnyLang In)
      ScriptDatumOrFileSpending
      -- ^ Optional Datum (CIP-69)
      ScriptDataOrFile
      -- ^ Redeemer
      ExecutionUnits
  | OnDiskSimpleCliArgs
      (File ScriptInAnyLang In)
  deriving Show

createSimpleOrPlutusScriptFromCliArgs
  :: File ScriptInAnyLang In
  -> Maybe (ScriptDatumOrFileSpending, ScriptDataOrFile, ExecutionUnits)
  -> CliSpendScriptRequirements
createSimpleOrPlutusScriptFromCliArgs scriptFp (Just (datumFile, redeemerFile, execUnits)) =
  OnDiskSimpleOrPlutusScript $ OnDiskPlutusScriptCliArgs scriptFp datumFile redeemerFile execUnits
createSimpleOrPlutusScriptFromCliArgs scriptFp Nothing = OnDiskSimpleOrPlutusScript $ OnDiskSimpleCliArgs scriptFp

newtype SimpleRefScriptCliArgs = SimpleRefScriptArgs TxIn deriving Show

createSimpleReferenceScriptFromCliArgs :: TxIn -> CliSpendScriptRequirements
createSimpleReferenceScriptFromCliArgs = OnDiskSimpleRefScript . SimpleRefScriptArgs

data PlutusRefScriptCliArgs
  = PlutusRefScriptCliArgs
      TxIn
      -- ^ TxIn with reference script
      AnyPlutusScriptVersion
      ScriptDatumOrFileSpending
      -- ^ Optional Datum (CIP-69)
      ScriptDataOrFile
      -- ^ Redeemer
      ExecutionUnits
  deriving Show

createPlutusReferenceScriptFromCliArgs
  :: TxIn
  -> AnyPlutusScriptVersion
  -> ScriptDatumOrFileSpending
  -> ScriptDataOrFile
  -> ExecutionUnits
  -> CliSpendScriptRequirements
createPlutusReferenceScriptFromCliArgs txin v mDatum redeemer execUnits =
  OnDiskPlutusRefScript $ PlutusRefScriptCliArgs txin v mDatum redeemer execUnits

data ScriptDatumOrFileSpending
  = PotentialDatum (Maybe ScriptDataOrFile)
  | InlineDatum
  deriving Show
