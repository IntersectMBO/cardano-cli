{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Script.Types
  ( -- * Errors
    CliScriptWitnessError (..)
  )
where

import           Cardano.Api

import           Cardano.CLI.Types.Errors.PlutusScriptDecodeError
import           Cardano.CLI.Types.Errors.ScriptDataError
import           Cardano.CLI.Types.Errors.ScriptDecodeError

data CliScriptWitnessError
  = SimpleScriptWitnessDecodeError ScriptDecodeError
  | PlutusScriptWitnessDecodeError PlutusScriptDecodeError
  | PlutusScriptWitnessLanguageNotSupportedInEra
      AnyPlutusScriptVersion
      AnyShelleyBasedEra
  | PlutusScriptWitnessRedeemerError ScriptDataError

instance Error CliScriptWitnessError where
  prettyError = \case
    SimpleScriptWitnessDecodeError err -> prettyError err
    PlutusScriptWitnessDecodeError err -> prettyError err
    PlutusScriptWitnessLanguageNotSupportedInEra version era ->
      "Plutus script version " <> pshow version <> " is not supported in era " <> pshow era
    PlutusScriptWitnessRedeemerError err -> renderScriptDataError err
