{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.CLI.EraBased.Script.Type
  ( AnyPlutusScript (..)

    -- * New experimental api
  , PlutusScriptRequirements (..)
  , OnDiskPlutusScriptCliArgs (..)
  , PlutusRefScriptCliArgs (..)
  , OptionalDatum

    -- * Errors
  , CliScriptWitnessError (..)
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.PlutusScriptDecodeError
import Cardano.CLI.Type.Error.ScriptDataError
import Cardano.CLI.Type.Error.ScriptDecodeError

-- TODO: Move to cardano-api
data AnyPlutusScript where
  AnyPlutusScript
    :: IsPlutusScriptLanguage lang => PlutusScriptVersion lang -> PlutusScript lang -> AnyPlutusScript

data CliScriptWitnessError
  = SimpleScriptWitnessDecodeError ScriptDecodeError
  | TextEnvelopeError TextEnvelopeError
  | PlutusScriptWitnessDecodeError PlutusScriptDecodeError
  | PlutusScriptWitnessLanguageNotSupportedInEra
      AnyPlutusScriptVersion
      AnyShelleyBasedEra
  | PlutusScriptWitnessRedeemerError ScriptDataError
  deriving Show

instance Error CliScriptWitnessError where
  prettyError = \case
    SimpleScriptWitnessDecodeError err -> prettyError err
    TextEnvelopeError err -> prettyError err
    PlutusScriptWitnessDecodeError err -> prettyError err
    PlutusScriptWitnessLanguageNotSupportedInEra version era ->
      "Plutus script version " <> pshow version <> " is not supported in era " <> pshow era
    PlutusScriptWitnessRedeemerError err -> renderScriptDataError err

data PlutusScriptRequirements (witessable :: Exp.WitnessableItem) where
  OnDiskSimpleScript :: (File ScriptInAnyLang In) -> PlutusScriptRequirements witnessable
  OnDiskPlutusScript
    :: (OnDiskPlutusScriptCliArgs witnessable) -> PlutusScriptRequirements witnessable
  PlutusReferenceScript
    :: (PlutusRefScriptCliArgs witnessable) -> PlutusScriptRequirements witnessable
  SimpleReferenceScript :: SimpleRefScriptCliArgs -> PlutusScriptRequirements witnessable

deriving instance Show (PlutusScriptRequirements witnessable)

-- instance Show (PlutusScriptRequirements witnessable) where
--  show a@OnDiskSimpleScript{} = show a
--  show a@SimpleReferenceScript{} = show a
--  show a@OnDiskPlutusScript{} = show a
--  show a@PlutusReferenceScript{} = show a

type family OptionalDatum (a :: Exp.WitnessableItem) where
  OptionalDatum Exp.TxInItem = Maybe HashableScriptData
  OptionalDatum Exp.CertItem = Exp.NoScriptDatum
  OptionalDatum Exp.MintItem = Exp.NoScriptDatum
  OptionalDatum Exp.WithdrawalItem = Exp.NoScriptDatum
  OptionalDatum Exp.VoterItem = Exp.NoScriptDatum
  OptionalDatum Exp.ProposalItem = Exp.NoScriptDatum

data OnDiskPlutusScriptCliArgs (witessable :: Exp.WitnessableItem) where
  OnDiskPlutusScriptCliArgs
    :: (File ScriptInAnyLang In)
    -> (OptionalDatum witnessable)
    -- ^ Optional Datum (CIP-69)
    -> ScriptDataOrFile
    -- ^ Redeemer
    -> ExecutionUnits
    -> OnDiskPlutusScriptCliArgs witnessable

instance Show (OnDiskPlutusScriptCliArgs witnessable) where
  show = undefined

data PlutusRefScriptCliArgs (witessable :: Exp.WitnessableItem) where
  PlutusRefScriptCliArgs
    :: TxIn
    -- ^ TxIn with reference script
    -> AnyPlutusScriptVersion
    -> (OptionalDatum witnessable)
    -- ^ Optional Datum (CIP-69)
    -> ScriptDataOrFile
    -- ^ Redeemer
    -> ExecutionUnits
    -> PlutusRefScriptCliArgs witnessable

instance Show (PlutusRefScriptCliArgs witnessable) where
  show = undefined

newtype SimpleRefScriptCliArgs = SimpleRefScriptArgs TxIn deriving Show
