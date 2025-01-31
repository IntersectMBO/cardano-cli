{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Script.Types
  ( AnyPlutusScript (..)

    -- * Errors
  , CliScriptWitnessError (..)
  )
where

import           Cardano.Api

import           Cardano.CLI.Types.Errors.PlutusScriptDecodeError
import           Cardano.CLI.Types.Errors.ScriptDataError
import           Cardano.CLI.Types.Errors.ScriptDecodeError

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
