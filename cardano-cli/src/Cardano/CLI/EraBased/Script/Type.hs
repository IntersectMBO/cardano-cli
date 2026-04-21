{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Script.Type
  ( -- * Script requirements per context
    SimpleScriptRequirements (..)
  , PlutusSpendingScriptRequirements (..)
  , PlutusMintingScriptRequirements (..)
  , PlutusNonAssetScriptRequirements (..)

    -- * Command-level sum types
  , AnySpendScript (..)
  , AnyMintScript (..)
  , AnyNonAssetScript (..)

    -- * Datum
  , ScriptDatumOrFileSpending (..)

    -- * Errors
  , CliScriptWitnessError (..)
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Type.Common

data CliScriptWitnessError
  = PlutusScriptWitnessLanguageNotSupportedInEra
      L.Language
      AnyShelleyBasedEra
  deriving Show

instance Error CliScriptWitnessError where
  prettyError = \case
    PlutusScriptWitnessLanguageNotSupportedInEra version era ->
      "Plutus script version " <> pshow version <> " is not supported in era " <> pshow era

data ScriptDatumOrFileSpending
  = PotentialDatum (Maybe ScriptDataOrFile)
  | InlineDatum
  deriving Show

-- | Simple script provided either on disk or as a reference input.
-- Shared across all script contexts since simple scripts have no
-- context-specific fields.
data SimpleScriptRequirements
  = OnDiskSimpleScript (File ScriptInAnyLang In)
  | ReferenceSimpleScript TxIn
  deriving Show

-- | Plutus script requirements for spending. Spending is the only context
-- that carries an optional datum (CIP-69).
data PlutusSpendingScriptRequirements
  = OnDiskPlutusSpendingScript
      (File ScriptInAnyLang In)
      ScriptDatumOrFileSpending
      -- ^ Optional Datum (CIP-69)
      ScriptDataOrFile
      -- ^ Redeemer
      ExecutionUnits
  | ReferencePlutusSpendingScript
      TxIn
      -- ^ TxIn with reference script
      AnySLanguage
      ScriptDatumOrFileSpending
      -- ^ Optional Datum (CIP-69)
      ScriptDataOrFile
      -- ^ Redeemer
      ExecutionUnits
  deriving Show

-- | Plutus script requirements for minting. Minting requires a 'PolicyId' so
-- the CLI can validate that every policy being minted has a corresponding
-- script witness and vice versa (see @createTxMintValue@). For on-disk scripts
-- the 'PolicyId' is computed from the script hash; for reference scripts the
-- user must supply it because the script bytes aren't available locally.
data PlutusMintingScriptRequirements
  = OnDiskPlutusMintingScript
      (File ScriptInAnyLang In)
      ScriptDataOrFile
      -- ^ Redeemer
      ExecutionUnits
  | ReferencePlutusMintingScript
      TxIn
      -- ^ TxIn with reference script
      AnySLanguage
      PolicyId
      -- ^ Needed for plutus minting scripts
      ScriptDataOrFile
      -- ^ Redeemer
      ExecutionUnits
  deriving Show

-- | Plutus script requirements for non-asset contexts (certificates, voting,
-- withdrawals, proposals). These contexts need neither a datum nor a policy id,
-- so they share a single type.
data PlutusNonAssetScriptRequirements
  = OnDiskPlutusNonAssetScript
      (File ScriptInAnyLang In)
      ScriptDataOrFile
      -- ^ Redeemer
      ExecutionUnits
  | ReferencePlutusNonAssetScript
      TxIn
      -- ^ TxIn with reference script
      AnySLanguage
      ScriptDataOrFile
      -- ^ Redeemer
      ExecutionUnits
  deriving Show

-- | A spending script witness — simple or Plutus.
data AnySpendScript
  = AnySpendScriptSimple SimpleScriptRequirements
  | AnySpendScriptPlutus PlutusSpendingScriptRequirements
  deriving Show

-- | A minting script witness — simple or Plutus.
-- Simple minting scripts are split into on-disk and reference variants because
-- the 'PolicyId' for on-disk scripts is computed from the script hash at read
-- time, while reference scripts require the user to supply it via the CLI.
data AnyMintScript
  = AnyMintScriptSimpleOnDisk (File ScriptInAnyLang In)
  | AnyMintScriptSimpleRef TxIn PolicyId
  | AnyMintScriptPlutus PlutusMintingScriptRequirements
  deriving Show

-- | A non-asset script witness — simple or Plutus.
-- Used for certificates, voting, withdrawals, and proposals.
data AnyNonAssetScript
  = AnyNonAssetScriptSimple SimpleScriptRequirements
  | AnyNonAssetScriptPlutus PlutusNonAssetScriptRequirements
  deriving Show
