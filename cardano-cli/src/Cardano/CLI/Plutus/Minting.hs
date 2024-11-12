{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

{-
The types defined in the module is a result of how the parsers have been implemented.

These obviously collapse to the same type: pMintingScript <|> pSimpleReferenceMintingScriptWitness <|> pPlutusMintReferenceScriptWitnessFiles balanceExecUnits
However the reference script parsing options require the PolicyId of the script. If the script or policy id
is not provided (i.e we are using a reference script) then we have to fetch the script and calculate the policy id.
-}
module Cardano.CLI.Plutus.Minting
  ( AnyMintScriptFiles (..)
  , MintScriptLocationOnDisk (..)
  , MintScriptLocationOffDisk (..)
  , PlutusMintScriptWitnessFiles (..)
  , SimpleMintScriptWitnessFiles (..)
  , CliMintScriptRequirements (..)
  , createAnyOnDiskMintScriptFiles
  , createOnDiskSimpleOfPlutusScriptCliArgs
  , createOnDiskSimpleReferenceScriptCliArgs
  , createOnDiskPlutusReferenceScriptCliArgs
  , createSimpleMintScriptWitnessFiles
  )
where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common (ScriptDataOrFile)

import           Data.Typeable

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

-- The policy id is needed in transaction construction wrt minting
data MintingScriptWitnessWithPolId era
  = MintingScriptWitnessWithPolId
      PolicyId
      (ScriptWitness WitCtxMint era)
  deriving Show

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

data OnDiskSimpleOrPlutusScriptCliArgs
  = OnDiskSimpleScriptCliArgs
      (File ScriptInAnyLang In)
  | OnDiskPlutusScriptCliArgs
      (File ScriptInAnyLang In)
      ScriptDataOrFile
      ExecutionUnits

createOnDiskSimpleOfPlutusScriptCliArgs
  :: File ScriptInAnyLang In
  -> Maybe (ScriptDataOrFile, ExecutionUnits)
  -> CliMintScriptRequirements
createOnDiskSimpleOfPlutusScriptCliArgs scriptFp Nothing =
  OnDiskSimpleOrPlutusScript $ OnDiskSimpleScriptCliArgs scriptFp
createOnDiskSimpleOfPlutusScriptCliArgs scriptFp (Just (redeemerFile, execUnits)) =
  OnDiskSimpleOrPlutusScript $ OnDiskPlutusScriptCliArgs scriptFp redeemerFile execUnits

data OnDiskSimpleRefScriptCliArgs
  = OnDiskSimpleRefScriptCliArgs
      TxIn
      PolicyId

createOnDiskSimpleReferenceScriptCliArgs
  :: TxIn
  -> PolicyId
  -> CliMintScriptRequirements
createOnDiskSimpleReferenceScriptCliArgs txin polid =
  OnDiskSimpleRefScript $ OnDiskSimpleRefScriptCliArgs txin polid

data OnDiskPlutusRefScriptCliArgs
  = OnDiskPlutusRefScriptCliArgs
      TxIn
      AnyPlutusScriptVersion
      ScriptDataOrFile
      ExecutionUnits
      PolicyId

createOnDiskPlutusReferenceScriptCliArgs
  :: TxIn
  -> AnyPlutusScriptVersion
  -> ScriptDataOrFile
  -> ExecutionUnits
  -> PolicyId
  -> CliMintScriptRequirements
createOnDiskPlutusReferenceScriptCliArgs txin scriptVersion scriptData execUnits polid =
  OnDiskPlutusRefScript $ OnDiskPlutusRefScriptCliArgs txin scriptVersion scriptData execUnits polid

data CliMintScriptRequirements
  = OnDiskSimpleOrPlutusScript OnDiskSimpleOrPlutusScriptCliArgs
  | OnDiskSimpleRefScript OnDiskSimpleRefScriptCliArgs
  | OnDiskPlutusRefScript OnDiskPlutusRefScriptCliArgs

data NewScriptWitnessError
  = SimpleScriptWitnessDecodeError ScriptDecodeError
  | PlutusScriptWitnessDecodeError PlutusScriptDecodeError
  | PlutusScriptWitnessLanguageNotSupportedInEra
      AnyPlutusScriptVersion
      AnyShelleyBasedEra
  | PlutusScriptWitnessRedeemerError ScriptDataError

data FileErrorOrNot -- <-- TODO
-- When dealing with a reference script you do not have access to a script filepath!

readScriptWitness
  :: MonadIOTransError (FileError NewScriptWitnessError) t m
  => Typeable era
  => ShelleyBasedEra era -> CliMintScriptRequirements -> t m (MintScriptWitWithPolId era)
readScriptWitness sbe (OnDiskSimpleOrPlutusScript simpleOrPlutus) =
  case simpleOrPlutus of
    OnDiskSimpleScriptCliArgs simpleFp -> do
      let sFp = unFile simpleFp
      s <-
        modifyError (fmap SimpleScriptWitnessDecodeError) $ readFileSimpleScript sFp
      case s of
        SimpleScript ss -> do
          let polId = PolicyId $ hashScript s
          return $
            MintScriptWitWithPolId polId $
              SimpleScriptWitness (sbeToSimpleScriptLangInEra sbe) $
                SScript ss
    OnDiskPlutusScriptCliArgs plutusScriptFp redeemerFile execUnits -> do
      let sFp = unFile plutusScriptFp
      plutusScript <-
        modifyError (fmap PlutusScriptWitnessDecodeError) $
          readFilePlutusScript $
            unFile plutusScriptFp

      redeemer <-
        modifyError (FileError sFp . PlutusScriptWitnessRedeemerError) $
          readScriptDataOrFile redeemerFile
      case plutusScript of
        AnyPlutusScript lang script -> do
          let pScript = PScript script
              polId = PolicyId $ hashScript $ PlutusScript lang script
          sLangSupported <-
            modifyError (FileError sFp)
              $ hoistMaybe
                ( PlutusScriptWitnessLanguageNotSupportedInEra
                    (AnyPlutusScriptVersion lang)
                    (AnyShelleyBasedEra sbe)
                )
              $ scriptLanguageSupportedInEra sbe
              $ PlutusScriptLanguage lang
          return $
            MintScriptWitWithPolId polId $
              PlutusScriptWitness
                sLangSupported
                lang
                pScript
                NoScriptDatumForMint
                redeemer
                execUnits
readScriptWitness sbe (OnDiskSimpleRefScript (OnDiskSimpleRefScriptCliArgs refTxIn polId)) =
  return $
    MintScriptWitWithPolId polId $
      SimpleScriptWitness
        (sbeToSimpleScriptLangInEra sbe)
        (SReferenceScript refTxIn $ Just $ unPolicyId polId)
readScriptWitness
  sbe
  ( OnDiskPlutusRefScript
      (OnDiskPlutusRefScriptCliArgs refTxIn anyPlutusScriptVersion redeemerFile execUnits polId)
    ) = do
    case anyPlutusScriptVersion of
      AnyPlutusScriptVersion lang -> do
        let pScript = PReferenceScript refTxIn $ Just $ unPolicyId polId
        redeemer <-
          modifyError (FileError "TODO: Not a suitable error" . PlutusScriptWitnessRedeemerError) $
            readScriptDataOrFile redeemerFile
        sLangSupported <-
          modifyError (FileError "TODO: Not a suitable error")
            $ hoistMaybe
              ( PlutusScriptWitnessLanguageNotSupportedInEra
                  (AnyPlutusScriptVersion lang)
                  (AnyShelleyBasedEra sbe)
              )
            $ scriptLanguageSupportedInEra sbe
            $ PlutusScriptLanguage lang
        return $
          MintScriptWitWithPolId polId $
            PlutusScriptWitness
              sLangSupported
              lang
              pScript
              NoScriptDatumForMint
              redeemer
              execUnits

sbeToSimpleScriptLangInEra
  :: ShelleyBasedEra era -> ScriptLanguageInEra SimpleScript' era
sbeToSimpleScriptLangInEra ShelleyBasedEraShelley = SimpleScriptInShelley
sbeToSimpleScriptLangInEra ShelleyBasedEraAllegra = SimpleScriptInAllegra
sbeToSimpleScriptLangInEra ShelleyBasedEraMary = SimpleScriptInMary
sbeToSimpleScriptLangInEra ShelleyBasedEraAlonzo = SimpleScriptInAlonzo
sbeToSimpleScriptLangInEra ShelleyBasedEraBabbage = SimpleScriptInBabbage
sbeToSimpleScriptLangInEra ShelleyBasedEraConway = SimpleScriptInConway

-- In the case of reference scripts, the user currently must provide the policy id (script hash)
-- in order to correctly construct the transaction.
data MintScriptWitWithPolId era
  = MintScriptWitWithPolId
      PolicyId
      (ScriptWitness WitCtxMint era)
  deriving Show
