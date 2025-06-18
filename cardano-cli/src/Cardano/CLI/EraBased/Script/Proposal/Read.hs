{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Script.Proposal.Read
  ( readProposalScriptWitness
  )
where

import Cardano.Api
import Cardano.Api.Experimental (obtainCommonConstraints)
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.EraBased.Script.Proposal.Type
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Type.Common

readProposalScriptWitness
  :: forall t m era
   . ( MonadIOTransError (FileError CliScriptWitnessError) t m
     , Exp.IsEra era
     )
  => (ProposalFile In, Maybe CliProposalScriptRequirements)
  -> t m (Proposal era, Maybe (ProposalScriptWitness era))
readProposalScriptWitness (propFp, Nothing) = do
  proposal <-
    obtainCommonConstraints (Exp.useEra @era) $
      modifyError (fmap TextEnvelopeError) $
        hoistIOEither $
          readFileTextEnvelope propFp
  return (proposal, Nothing)
readProposalScriptWitness (propFp, Just certScriptReq) = do
  let sbe = convert Exp.useEra
  proposal <-
    obtainCommonConstraints (Exp.useEra @era) $
      modifyError (fmap TextEnvelopeError) $
        hoistIOEither $
          readFileTextEnvelope propFp
  case certScriptReq of
    OnDiskSimpleScript scriptFp -> do
      let sFp = unFile scriptFp
      s <-
        modifyError (fmap SimpleScriptWitnessDecodeError) $
          readFileSimpleScript sFp
      case s of
        SimpleScript ss -> do
          return
            ( proposal
            , Just $
                ProposalScriptWitness
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
            ( proposal
            , Just $
                ProposalScriptWitness $
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
            ( proposal
            , Just $
                ProposalScriptWitness $
                  PlutusScriptWitness
                    sLangSupported
                    lang
                    pScript
                    NoScriptDatumForStake
                    redeemer
                    execUnits
            )
