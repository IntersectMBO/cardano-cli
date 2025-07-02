{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.CLI.EraBased.Script.Type
  ( AnyPlutusScript (..)

    -- * New experimental api
  , ScriptRequirements (..)
  , OnDiskPlutusScriptCliArgs (..)
  , PlutusRefScriptCliArgs (..)
  , MintPolicyId
  , NoPolicyId (..)
  , OptionalDatum
  , SimpleRefScriptCliArgs (..)
  , ScriptDatumOrFileSpending (..)

    -- * Errors
  , CliScriptWitnessError (..)
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.Type.Common

-- TODO: Move to cardano-api
data AnyPlutusScript where
  AnyPlutusScript
    :: IsPlutusScriptLanguage lang => PlutusScriptVersion lang -> PlutusScript lang -> AnyPlutusScript

data CliScriptWitnessError
  = PlutusScriptWitnessLanguageNotSupportedInEra
      AnyPlutusScriptVersion
      AnyShelleyBasedEra
  deriving Show

instance Error CliScriptWitnessError where
  prettyError = \case
    PlutusScriptWitnessLanguageNotSupportedInEra version era ->
      "Plutus script version " <> pshow version <> " is not supported in era " <> pshow era

-- | Encapsulates the requirements for a simple or plutus script being read from disk.
data ScriptRequirements (witnessable :: Exp.WitnessableItem) where
  OnDiskSimpleScript :: File ScriptInAnyLang In -> ScriptRequirements witnessable
  OnDiskPlutusScript
    :: OnDiskPlutusScriptCliArgs witnessable -> ScriptRequirements witnessable
  PlutusReferenceScript
    :: PlutusRefScriptCliArgs witnessable -> ScriptRequirements witnessable
  SimpleReferenceScript
    :: SimpleRefScriptCliArgs witnessable -> ScriptRequirements witnessable

deriving instance Show (ScriptRequirements Exp.VoterItem)

deriving instance Show (ScriptRequirements Exp.MintItem)

deriving instance Show (ScriptRequirements Exp.CertItem)

deriving instance Show (ScriptRequirements Exp.TxInItem)

deriving instance Show (ScriptRequirements Exp.ProposalItem)

deriving instance Show (ScriptRequirements Exp.WithdrawalItem)

data OnDiskPlutusScriptCliArgs (witnessable :: Exp.WitnessableItem) where
  OnDiskPlutusScriptCliArgs
    :: (File ScriptInAnyLang In)
    -> (OptionalDatum witnessable)
    -- ^ Optional Datum (CIP-69)
    -> ScriptDataOrFile
    -- ^ Redeemer
    -> ExecutionUnits
    -> OnDiskPlutusScriptCliArgs witnessable

type family OptionalDatum (a :: Exp.WitnessableItem) where
  OptionalDatum Exp.TxInItem = ScriptDatumOrFileSpending
  OptionalDatum Exp.CertItem = Exp.NoScriptDatum
  OptionalDatum Exp.MintItem = Exp.NoScriptDatum
  OptionalDatum Exp.WithdrawalItem = Exp.NoScriptDatum
  OptionalDatum Exp.VoterItem = Exp.NoScriptDatum
  OptionalDatum Exp.ProposalItem = Exp.NoScriptDatum

data ScriptDatumOrFileSpending
  = PotentialDatum (Maybe ScriptDataOrFile)
  | InlineDatum
  deriving Show

deriving instance Show (OnDiskPlutusScriptCliArgs Exp.VoterItem)

deriving instance Show (OnDiskPlutusScriptCliArgs Exp.MintItem)

deriving instance Show (OnDiskPlutusScriptCliArgs Exp.ProposalItem)

deriving instance Show (OnDiskPlutusScriptCliArgs Exp.CertItem)

deriving instance Show (OnDiskPlutusScriptCliArgs Exp.TxInItem)

deriving instance Show (OnDiskPlutusScriptCliArgs Exp.WithdrawalItem)

data PlutusRefScriptCliArgs (witnessable :: Exp.WitnessableItem) where
  PlutusRefScriptCliArgs
    :: TxIn
    -- ^ TxIn with reference script
    -> AnyPlutusScriptVersion
    -> OptionalDatum witnessable
    -- ^ Optional Datum (CIP-69)
    -> MintPolicyId witnessable
    -- ^ Needed for plutus minting scripts
    -> ScriptDataOrFile
    -- ^ Redeemer
    -> ExecutionUnits
    -> PlutusRefScriptCliArgs witnessable

deriving instance Show (PlutusRefScriptCliArgs Exp.VoterItem)

deriving instance Show (PlutusRefScriptCliArgs Exp.MintItem)

deriving instance Show (PlutusRefScriptCliArgs Exp.ProposalItem)

deriving instance Show (PlutusRefScriptCliArgs Exp.CertItem)

deriving instance Show (PlutusRefScriptCliArgs Exp.TxInItem)

deriving instance Show (PlutusRefScriptCliArgs Exp.WithdrawalItem)

data SimpleRefScriptCliArgs witnessable where
  SimpleRefScriptArgs :: TxIn -> MintPolicyId witnessable -> SimpleRefScriptCliArgs witnessable

deriving instance Show (SimpleRefScriptCliArgs Exp.VoterItem)

deriving instance Show (SimpleRefScriptCliArgs Exp.MintItem)

deriving instance Show (SimpleRefScriptCliArgs Exp.ProposalItem)

deriving instance Show (SimpleRefScriptCliArgs Exp.CertItem)

deriving instance Show (SimpleRefScriptCliArgs Exp.TxInItem)

deriving instance Show (SimpleRefScriptCliArgs Exp.WithdrawalItem)

type family MintPolicyId (a :: Exp.WitnessableItem) where
  MintPolicyId Exp.TxInItem = NoPolicyId
  MintPolicyId Exp.CertItem = NoPolicyId
  MintPolicyId Exp.MintItem = PolicyId
  MintPolicyId Exp.WithdrawalItem = NoPolicyId
  MintPolicyId Exp.VoterItem = NoPolicyId
  MintPolicyId Exp.ProposalItem = NoPolicyId

data NoPolicyId = NoPolicyId deriving Show
