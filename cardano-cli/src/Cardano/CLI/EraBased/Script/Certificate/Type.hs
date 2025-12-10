{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Cardano.CLI.EraBased.Script.Certificate.Type
  ( CertificateScriptWitness (..)
  , PlutusRefScriptCliArgs (..)
  , createSimpleOrPlutusScriptFromCliArgs
  , createPlutusReferenceScriptFromCliArgs
  )
where

import Cardano.Api
import Cardano.Api.Experimental

import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Type.Common (AnySLanguage (..), ScriptDataOrFile)

newtype CertificateScriptWitness era
  = CertificateScriptWitness {cswScriptWitness :: ScriptWitness WitCtxStake era}
  deriving Show

createSimpleOrPlutusScriptFromCliArgs
  :: File ScriptInAnyLang In
  -> Maybe (ScriptDataOrFile, ExecutionUnits)
  -> ScriptRequirements CertItem
createSimpleOrPlutusScriptFromCliArgs scriptFp (Just (redeemer, execUnits)) =
  OnDiskPlutusScript $ OnDiskPlutusScriptCliArgs scriptFp NoScriptDatumAllowed redeemer execUnits
createSimpleOrPlutusScriptFromCliArgs scriptFp Nothing =
  OnDiskSimpleScript scriptFp

createPlutusReferenceScriptFromCliArgs
  :: TxIn
  -> AnySLanguage
  -> ScriptDataOrFile
  -> ExecutionUnits
  -> ScriptRequirements CertItem
createPlutusReferenceScriptFromCliArgs txIn l redeemer execUnits =
  PlutusReferenceScript $
    PlutusRefScriptCliArgs txIn l NoScriptDatumAllowed NoPolicyId redeemer execUnits
