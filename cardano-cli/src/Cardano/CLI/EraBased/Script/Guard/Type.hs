{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.CLI.EraBased.Script.Guard.Type
  ( Guard (..)
  , GuardScriptSource (..)
  , TxGuardArguments (..)
  , TxGuardRequirements (..)
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.Type.Common

-- | The guard credential is derived from a file or supplied with a reference input.
data GuardScriptSource
  = GuardScriptOnDisk ScriptFile
  | GuardScriptReference TxIn ScriptHash
  deriving Show

-- | A Dijkstra guard supplied on the command line. Files are read before
-- constructing the API's unified 'Exp.TxGuards' collection.
data Guard
  = KeyGuard RequiredSigner
  | ScriptGuard GuardScriptSource (Maybe (ScriptDataOrFile, ExecutionUnits))
  deriving Show

-- | Conway accepts required signers; Dijkstra accepts one list of key and script guards.
data TxGuardArguments era where
  ConwayRequiredSignerArguments :: [RequiredSigner] -> TxGuardArguments Exp.ConwayEra
  DijkstraGuardArguments :: [Guard] -> TxGuardArguments Exp.DijkstraEra

deriving instance Show (TxGuardArguments era)

-- | Resolved requirements passed to the transaction builders. Dijkstra keeps
-- key and script guards together in the API's validated collection.
data TxGuardRequirements era where
  ConwayRequiredSigners :: [Hash PaymentKey] -> TxGuardRequirements Exp.ConwayEra
  DijkstraGuards
    :: Exp.TxGuards (Exp.LedgerEra Exp.DijkstraEra)
    -> TxGuardRequirements Exp.DijkstraEra
