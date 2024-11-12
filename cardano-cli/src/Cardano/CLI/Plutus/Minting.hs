{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.CLI.Plutus.Minting
  ( AnyMintScriptFiles (..)
  , MintScriptLocationOnDisk (..)
  , MintScriptLocationOffDisk (..)
  , PlutusMintScriptWitnessFiles (..)
  , SimpleMintScriptWitnessFiles (..)
  , createAnyOnDiskMintScriptFiles
  , createSimpleMintScriptWitnessFiles
  , readAnyMintScriptFiles
  )
where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common

data AnyMintScriptFiles
  = PlutusMintScriptFiles PlutusMintScriptWitnessFiles
  | SimpleMintScriptFiles SimpleMintScriptWitnessFiles
  deriving Show

createAnyOnDiskMintScriptFiles
  :: MintScriptLocationOnDisk
  -> Maybe (ScriptDataOrFile, ExecutionUnits)
  -> AnyMintScriptFiles
createAnyOnDiskMintScriptFiles scriptLocation Nothing = SimpleMintScriptFiles $ SimpleMintOnDisk scriptLocation
createAnyOnDiskMintScriptFiles scriptLocation (Just sdata) =
  PlutusMintScriptFiles $ createPlutusMintScriptWitnessFiles scriptLocation sdata

readAnyMintScriptFiles
  :: MonadIOTransError (FileError ScriptDecodeError) t m
  => MonadFail (t m)
  => ShelleyBasedEra era -> [AnyMintScriptFiles] -> t m [ScriptWitness WitCtxMint era]
readAnyMintScriptFiles era =
  mapM (readPlutusMintScriptWitness era)

newtype MintScriptLocationOnDisk
  = ScriptInFile (File ScriptInAnyLang In)
  deriving Show

data MintScriptLocationOffDisk where
  ReferenceScriptNodeAccess :: TxIn -> MintScriptLocationOffDisk
  RefenceScriptNoNodeAcccess
    :: TxIn
    -> PolicyId
    -> ScriptLanguage lang
    -> MintScriptLocationOffDisk

-- No way to determine the language because you do not have the script!!
-- However in the simple script case we default to timelock scripts (so we can put Nothing)
-- In plutus case user must specify script language

deriving instance Show MintScriptLocationOffDisk

data PlutusMintScriptWitnessFiles
  = PlutusMintOnDisk
      MintScriptLocationOnDisk
      ScriptDataOrFile
      ExecutionUnits
  | PlutusMintOffDisk
      MintScriptLocationOffDisk
      ScriptDataOrFile
      ExecutionUnits
  deriving Show

data MintingScriptWitnessWithPolId era
  = MintingScriptWitnessWithPolId
      PolicyId
      (ScriptWitness WitCtxMint era)
  deriving Show

readPlutusMintScriptWitness
  :: MonadIOTransError (FileError ScriptDecodeError) t m
  => MonadFail (t m)
  => ShelleyBasedEra era
  -> AnyMintScriptFiles
  -> t m (MintingScriptWitnessWithPolId era)
readPlutusMintScriptWitness _sbe (SimpleMintScriptFiles onOrOffDisk) =
  case onOrOffDisk of
    SimpleMintOffDisk (ReferenceScriptNodeAccess txin) ->
      undefined
    SimpleMintOffDisk (RefenceScriptNoNodeAcccess txin _policyId _scriptLang) ->
      undefined
    SimpleMintOnDisk (ScriptInFile scriptFile) -> do
      script@(SimpleScript s) <- readFileSimpleScript $ unFile scriptFile
      return $
        MintingScriptWitnessWithPolId (scriptPolicyId script) $
          SimpleScriptWitness (error "TODO") (SScript s)
-- script <- readFileTextEnvelope (AsScriptInAnyLang AsMintingScript) scriptFile
-- return $ SimpleScriptWitness script
readPlutusMintScriptWitness _sbe (PlutusMintScriptFiles onOrOffDisk) =
  case onOrOffDisk of
    (PlutusMintOnDisk _scriptLocation _sdata _execUnits) -> undefined
    (PlutusMintOffDisk _scriptLocation _sdata _execUnits) -> undefined

readSimpleMintScriptWitness
  :: MonadIOTransError (FileError ScriptDecodeError) t m
  => MonadFail (t m)
  => ShelleyBasedEra era
  -> SimpleMintScriptWitnessFiles
  -> t m (ScriptWitness WitCtxMint era)
readSimpleMintScriptWitness _sbe onOrOffDisk =
  case onOrOffDisk of
    SimpleMintOffDisk{} -> undefined
    SimpleMintOnDisk (ScriptInFile scriptFile) -> do
      SimpleScript s <- readFileSimpleScript $ unFile scriptFile
      return $ SimpleScriptWitness (error "TODO") (SScript s)

data SimpleMintScriptWitnessFiles
  = SimpleMintOnDisk MintScriptLocationOnDisk
  | SimpleMintOffDisk MintScriptLocationOffDisk
  deriving Show

createSimpleMintScriptWitnessFiles :: MintScriptLocationOnDisk -> SimpleMintScriptWitnessFiles
createSimpleMintScriptWitnessFiles = SimpleMintOnDisk

createPlutusMintScriptWitnessFiles
  :: MintScriptLocationOnDisk
  -> (ScriptDataOrFile, ExecutionUnits)
  -> PlutusMintScriptWitnessFiles
createPlutusMintScriptWitnessFiles scriptLocation (sRedeemer, execUnits) =
  PlutusMintOnDisk scriptLocation sRedeemer execUnits

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
