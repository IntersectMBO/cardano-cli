{-# LANGUAGE DataKinds #-}

module Cardano.CLI.EraBased.Script.Mint.Type
  ( createSimpleOrPlutusScriptFromCliArgs
  , createPlutusReferenceScriptFromCliArgs
  , createSimpleReferenceScriptFromCliArgs
  , MintScriptWitnessWithPolicyId (..)
  )
where

import Cardano.Api

import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Type.Common (AnySLanguage (..), ScriptDataOrFile)

-- We always need the policy id when constructing a transaction that mints.
-- In the case of reference scripts, the user currently must provide the policy id (script hash)
-- in order to correctly construct the transaction.
data MintScriptWitnessWithPolicyId era
  = MintScriptWitnessWithPolicyId
  { mswPolId :: PolicyId
  , mswScriptWitness :: ScriptWitness WitCtxMint era
  }
  deriving Show

createSimpleOrPlutusScriptFromCliArgs
  :: File ScriptInAnyLang In
  -> Maybe (ScriptDataOrFile, ExecutionUnits)
  -> AnyMintScript
createSimpleOrPlutusScriptFromCliArgs scriptFp Nothing =
  AnyMintScriptSimpleOnDisk scriptFp
createSimpleOrPlutusScriptFromCliArgs scriptFp (Just (redeemerFile, execUnits)) =
  AnyMintScriptPlutus $ OnDiskPlutusMintingScript scriptFp redeemerFile execUnits

createSimpleReferenceScriptFromCliArgs
  :: TxIn
  -> PolicyId
  -> AnyMintScript
createSimpleReferenceScriptFromCliArgs txin polId =
  AnyMintScriptSimpleRef txin polId

createPlutusReferenceScriptFromCliArgs
  :: TxIn
  -> AnySLanguage
  -> ScriptDataOrFile
  -> ExecutionUnits
  -> PolicyId
  -> AnyMintScript
createPlutusReferenceScriptFromCliArgs txin scriptVersion scriptData execUnits polId =
  AnyMintScriptPlutus $
    ReferencePlutusMintingScript txin scriptVersion polId scriptData execUnits
