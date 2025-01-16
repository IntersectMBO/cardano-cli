{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Script.Spend.Types
  ( CliSpendScriptRequirements(..)
  , PlutusRefScriptCliArgs(..)
  , SimpleOrPlutusScriptCliArgs(..)
  , ScriptDatumOrFileSpending(..)
  , SimpleRefScriptCliArgs(..)
  , SpendScriptWitness(..)
  , createPlutusScriptFromCliArgs
  , createPlutusReferenceScriptFromCliArgs
  , createSimpleReferenceScriptFromCliArgs
  )
where

import           Cardano.Api

import           Cardano.CLI.Types.Common (ScriptDataOrFile)

data SpendScriptWitness era 
  = SpendScriptWitness { sswScriptWitness :: ScriptWitness WitCtxTxIn era }
  deriving Show

data CliSpendScriptRequirements 
  = OnDiskSimpleOrPlutusScript SimpleOrPlutusScriptCliArgs
  | OnDiskSimpleRefScript SimpleRefScriptCliArgs
  | OnDiskPlutusRefScript PlutusRefScriptCliArgs
  deriving Show


data SimpleOrPlutusScriptCliArgs
  =  OnDiskPlutusScriptCliArgs
      (File ScriptInAnyLang In)
      (ScriptDatumOrFileSpending) -- ^ Optional Datum (CIP-69)
      ScriptDataOrFile -- ^ Redeemer 
      ExecutionUnits
  | OnDiskSimpleOrPlutusScriptCliArgs
            (File ScriptInAnyLang In)

  deriving Show 

    
createPlutusScriptFromCliArgs 
  :: File ScriptInAnyLang In
  -> Maybe (ScriptDatumOrFileSpending, ScriptDataOrFile, ExecutionUnits)
  -> CliSpendScriptRequirements
createPlutusScriptFromCliArgs scriptFp (Just (datumFile, redeemerFile, execUnits)) = 
  OnDiskSimpleOrPlutusScript $ OnDiskPlutusScriptCliArgs scriptFp datumFile redeemerFile execUnits
createPlutusScriptFromCliArgs scriptFp Nothing = OnDiskSimpleOrPlutusScript $ OnDiskSimpleOrPlutusScriptCliArgs scriptFp


newtype SimpleRefScriptCliArgs = SimpleRefScriptArgs TxIn deriving Show 

createSimpleReferenceScriptFromCliArgs :: TxIn -> CliSpendScriptRequirements
createSimpleReferenceScriptFromCliArgs = OnDiskSimpleRefScript . SimpleRefScriptArgs

-- TODO: How to handle ScriptDatumOrFile type? You need to express that the datum
-- could also be inline!
data PlutusRefScriptCliArgs 
  = PlutusRefScriptCliArgs 
      TxIn -- ^ TxIn with reference script 
      AnyPlutusScriptVersion
      ScriptDatumOrFileSpending -- ^ Optional Datum (CIP-69)
      ScriptDataOrFile -- ^ Redeemer
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


