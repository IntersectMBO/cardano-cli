{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Plutus.Minting
  ( CliMintScriptRequirements (..)
  , MintScriptWitWithPolId (..)
  , createOnDiskSimpleOfPlutusScriptCliArgs
  , createOnDiskSimpleReferenceScriptCliArgs
  , createOnDiskPlutusReferenceScriptCliArgs
  , CliScriptWitnessError
  , readMintScriptWitness
  )
where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common (ScriptDataOrFile)

-- We always need the policy id when constructing a transaction that mints.
-- In the case of reference scripts, the user currently must provide the policy id (script hash)
-- in order to correctly construct the transaction.
data MintScriptWitWithPolId era
  = MintScriptWitWithPolId
  { mswPolId :: PolicyId
  , mswScriptWitness :: ScriptWitness WitCtxMint era
  }
  deriving Show

data OnDiskSimpleOrPlutusScriptCliArgs
  = OnDiskSimpleScriptCliArgs
      (File ScriptInAnyLang In)
  | OnDiskPlutusScriptCliArgs
      (File ScriptInAnyLang In)
      ScriptDataOrFile
      ExecutionUnits
  deriving Show

createOnDiskSimpleOfPlutusScriptCliArgs
  :: File ScriptInAnyLang In
  -> Maybe (ScriptDataOrFile, ExecutionUnits)
  -> CliMintScriptRequirements
createOnDiskSimpleOfPlutusScriptCliArgs scriptFp Nothing =
  OnDiskSimpleOrPlutusScript $ OnDiskSimpleScriptCliArgs scriptFp
createOnDiskSimpleOfPlutusScriptCliArgs scriptFp (Just (redeemerFile, execUnits)) =
  OnDiskSimpleOrPlutusScript $ OnDiskPlutusScriptCliArgs scriptFp redeemerFile execUnits

data SimpleRefScriptCliArgs
  = SimpleRefScriptCliArgs
      TxIn
      PolicyId
  deriving Show

createOnDiskSimpleReferenceScriptCliArgs
  :: TxIn
  -> PolicyId
  -> CliMintScriptRequirements
createOnDiskSimpleReferenceScriptCliArgs txin polid =
  OnDiskSimpleRefScript $ SimpleRefScriptCliArgs txin polid

data PlutusRefScriptCliArgs
  = PlutusRefScriptCliArgs
      TxIn
      AnyPlutusScriptVersion
      ScriptDataOrFile
      ExecutionUnits
      PolicyId
  deriving Show

createOnDiskPlutusReferenceScriptCliArgs
  :: TxIn
  -> AnyPlutusScriptVersion
  -> ScriptDataOrFile
  -> ExecutionUnits
  -> PolicyId
  -> CliMintScriptRequirements
createOnDiskPlutusReferenceScriptCliArgs txin scriptVersion scriptData execUnits polid =
  OnDiskPlutusRefScript $ PlutusRefScriptCliArgs txin scriptVersion scriptData execUnits polid

data CliMintScriptRequirements
  = OnDiskSimpleOrPlutusScript OnDiskSimpleOrPlutusScriptCliArgs
  | OnDiskSimpleRefScript SimpleRefScriptCliArgs
  | OnDiskPlutusRefScript PlutusRefScriptCliArgs
  deriving Show

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

readMintScriptWitness
  :: MonadIOTransError (FileError CliScriptWitnessError) t m
  => ShelleyBasedEra era -> CliMintScriptRequirements -> t m (MintScriptWitWithPolId era)
readMintScriptWitness sbe (OnDiskSimpleOrPlutusScript simpleOrPlutus) =
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
                    (shelleyBasedEraConstraints sbe $ AnyShelleyBasedEra sbe)
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
readMintScriptWitness sbe (OnDiskSimpleRefScript (SimpleRefScriptCliArgs refTxIn polId)) =
  return $
    MintScriptWitWithPolId polId $
      SimpleScriptWitness
        (sbeToSimpleScriptLangInEra sbe)
        (SReferenceScript refTxIn $ Just $ unPolicyId polId)
readMintScriptWitness
  sbe
  ( OnDiskPlutusRefScript
      (PlutusRefScriptCliArgs refTxIn anyPlutusScriptVersion redeemerFile execUnits polId)
    ) = do
    case anyPlutusScriptVersion of
      AnyPlutusScriptVersion lang -> do
        let pScript = PReferenceScript refTxIn $ Just $ unPolicyId polId
        redeemer <-
          -- TODO: Implement a new error type to capture this. FileError is not representative of cases
          -- where we do not have access to the script.
          modifyError (FileError "Reference script filepath not available" . PlutusScriptWitnessRedeemerError) $
            readScriptDataOrFile redeemerFile
        sLangSupported <-
          -- TODO: Implement a new error type to capture this. FileError is not representative of cases
          -- where we do not have access to the script.
          modifyError (FileError "Reference script filepath not available")
            $ hoistMaybe
              ( PlutusScriptWitnessLanguageNotSupportedInEra
                  (AnyPlutusScriptVersion lang)
                  (shelleyBasedEraConstraints sbe $ AnyShelleyBasedEra sbe)
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
