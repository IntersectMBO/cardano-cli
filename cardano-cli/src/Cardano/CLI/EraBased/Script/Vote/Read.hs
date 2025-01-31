{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Script.Vote.Read
  ( readVoteScriptWitness
  )
where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Script.Read.Common
import           Cardano.CLI.EraBased.Script.Types
import           Cardano.CLI.EraBased.Script.Vote.Types
import           Cardano.CLI.Types.Governance

readVoteScriptWitness
  :: MonadIOTransError (FileError CliScriptWitnessError) t m
  => ConwayEraOnwards era
  -> (VoteFile In, Maybe CliVoteScriptRequirements)
  -> t m (VotingProcedures era, Maybe (VoteScriptWitness era))
readVoteScriptWitness w (voteFp, Nothing) = do
  votProceds <-
    conwayEraOnwardsConstraints w $
      modifyError (fmap TextEnvelopeError) $
        hoistIOEither $
          readFileTextEnvelope AsVotingProcedures voteFp
  return (votProceds, Nothing)
readVoteScriptWitness w (voteFp, Just certScriptReq) = do
  let sbe = convert w
  votProceds <-
    conwayEraOnwardsConstraints w $
      modifyError (fmap TextEnvelopeError) $
        hoistIOEither $
          readFileTextEnvelope AsVotingProcedures voteFp
  case certScriptReq of
    OnDiskSimpleScript scriptFp -> do
      let sFp = unFile scriptFp
      s <-
        modifyError (fmap SimpleScriptWitnessDecodeError) $
          readFileSimpleScript sFp
      case s of
        SimpleScript ss -> do
          return
            ( votProceds
            , Just $
                VoteScriptWitness
                  ( SimpleScriptWitness (sbeToSimpleScriptLanguageInEra sbe) $
                      SScript ss
                  )
            )
    OnDiskPlutusScript (OnDiskPlutusScriptCliArgs scriptFp redeemerFile execUnits) -> do
      let plutusScriptFp = unFile scriptFp
      plutusScript <-
        modifyError (fmap PlutusScriptWitnessDecodeError) $
          readFilePlutusScript plutusScriptFp
      redeemer <-
        modifyError (FileError plutusScriptFp . PlutusScriptWitnessRedeemerError) $
          readScriptDataOrFile redeemerFile
      case plutusScript of
        AnyPlutusScript lang script -> do
          let pScript = PScript script
          sLangSupported <-
            modifyError (FileError plutusScriptFp)
              $ hoistMaybe
                ( PlutusScriptWitnessLanguageNotSupportedInEra
                    (AnyPlutusScriptVersion lang)
                    (shelleyBasedEraConstraints sbe $ AnyShelleyBasedEra sbe)
                )
              $ scriptLanguageSupportedInEra sbe
              $ PlutusScriptLanguage lang
          return
            ( votProceds
            , Just $
                VoteScriptWitness $
                  PlutusScriptWitness
                    sLangSupported
                    lang
                    pScript
                    NoScriptDatumForStake
                    redeemer
                    execUnits
            )
    OnDiskPlutusRefScript (PlutusRefScriptCliArgs refTxIn anyPlutusScriptVersion redeemerFile execUnits) -> do
      case anyPlutusScriptVersion of
        AnyPlutusScriptVersion lang -> do
          let pScript = PReferenceScript refTxIn
          redeemer <-
            -- TODO: Implement a new error type to capture this. FileError is not representative of cases
            -- where we do not have access to the script.
            modifyError
              ( FileError "Reference script filepath not available"
                  . PlutusScriptWitnessRedeemerError
              )
              $ readScriptDataOrFile redeemerFile
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

          return
            ( votProceds
            , Just $
                VoteScriptWitness $
                  PlutusScriptWitness
                    sLangSupported
                    lang
                    pScript
                    NoScriptDatumForStake
                    redeemer
                    execUnits
            )
