{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.CLI.Options.Plutus.Minting
  ( AnyMintScriptFiles (..)
  , MintScriptLocation (..)
  , PlutusMintScriptWitnessFiles (..)
  , createAnyMintScriptFiles
  , createSimpleMintScriptWitnessFiles
  )
where

import           Cardano.Api

import           Cardano.CLI.Types.Common

-- Depending on what you are witnessing changes the requirements
-- what about a different type based on what you are witnessing?

data AnyMintScriptFiles
  = PlutusMintScriptFiles PlutusMintScriptWitnessFiles
  | SimpleMintScriptFiles SimpleMintScriptWitnessFiles
  deriving Show

createAnyMintScriptFiles
  :: MintScriptLocation
  -> Maybe (ScriptDataOrFile, ExecutionUnits)
  -> AnyMintScriptFiles
createAnyMintScriptFiles scriptLocation Nothing = SimpleMintScriptFiles $ SimpleMint scriptLocation
createAnyMintScriptFiles scriptLocation (Just sdata) =
  PlutusMintScriptFiles $ createPlutusMintScriptWitnessFiles scriptLocation sdata

data MintScriptLocation where
  ScriptInFile :: File ScriptInAnyLang In -> MintScriptLocation
  ReferenceScriptNodeAccess :: TxIn -> MintScriptLocation
  RefenceScriptNoNodeAcccess
    :: TxIn
    -> PolicyId
    -> ScriptLanguage lang
    -> MintScriptLocation

-- No way to determine the language because you do not have the script!!
-- However in the simple script case we default to timelock scripts (so we can put Nothing)
-- In plutus case user must specify script language

deriving instance Show MintScriptLocation

data PlutusMintScriptWitnessFiles
  = PlutusMint
      MintScriptLocation
      ScriptDataOrFile
      ExecutionUnits
  deriving Show

newtype SimpleMintScriptWitnessFiles
  = SimpleMint MintScriptLocation
  deriving Show

createSimpleMintScriptWitnessFiles :: MintScriptLocation -> SimpleMintScriptWitnessFiles
createSimpleMintScriptWitnessFiles = SimpleMint

createPlutusMintScriptWitnessFiles
  :: MintScriptLocation
  -> (ScriptDataOrFile, ExecutionUnits)
  -> PlutusMintScriptWitnessFiles
createPlutusMintScriptWitnessFiles scriptLocation (sRedeemer, execUnits) =
  PlutusMint scriptLocation sRedeemer execUnits

-- TODO: Implement separate readScriptWitnessFiles
-- The problem is onnly the minting reference script
-- needs a policy id. So You can't modify ReferenceScript
-- to take a policy id because then it will be required for all
-- Maybe PolicyId is ugly. Maybe you can convert into GADT
-- and have a type level tag to restrict a third constructor
-- that can only be pattern matched in a minting context. However
-- now we have the problem of offline vs online. In offline mode
-- they have to provide the policy idd
-- We have to define new data definitions because now there is no longer a place
-- in  the PReferenceScript type to house a policy id.
-- Therefore we must define separate types. Where to draw the boundaries?
