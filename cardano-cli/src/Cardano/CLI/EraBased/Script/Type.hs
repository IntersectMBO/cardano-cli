{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Script.Type
  ( AnyPlutusScript (..)

    -- * Errors
  , CliScriptWitnessError (..)
  )
where

import Cardano.Api

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
