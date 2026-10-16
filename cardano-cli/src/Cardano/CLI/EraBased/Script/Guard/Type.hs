{-# LANGUAGE DataKinds #-}

module Cardano.CLI.EraBased.Script.Guard.Type (GuardScriptWitness (..), GuardScriptSource (..)) where

import Cardano.Api

import Cardano.CLI.Type.Common

-- | The guard credential is derived from a file or supplied with a reference input.
data GuardScriptSource
  = GuardScriptOnDisk ScriptFile
  | GuardScriptReference TxIn ScriptHash
  deriving Show

data GuardScriptWitness = GuardScriptWitness GuardScriptSource (Maybe (ScriptDataOrFile, ExecutionUnits))
  deriving Show
