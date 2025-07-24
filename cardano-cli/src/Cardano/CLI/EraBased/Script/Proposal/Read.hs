{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Script.Proposal.Read
  ( readProposal
  , readProposalScriptWitness
  , readTxGovernanceActions
  , ProposalError (..)
  )
where

import Cardano.Api
import Cardano.Api.Experimental (obtainCommonConstraints)
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Script.Proposal.Type
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Read
import Cardano.CLI.Type.Common

readProposalScriptWitness
  :: forall e era
   . Exp.IsEra era
  => (ProposalFile In, Maybe (ScriptRequirements Exp.ProposalItem))
  -> CIO e (Proposal era, Maybe (ProposalScriptWitness era))
readProposalScriptWitness (propFp, Nothing) = do
  proposal <-
    obtainCommonConstraints (Exp.useEra @era) $
      fromEitherIOCli @(FileError TextEnvelopeError) $
        readFileTextEnvelope propFp
  return (proposal, Nothing)
readProposalScriptWitness (propFp, Just certScriptReq) = do
  let sbe = convert Exp.useEra
  proposal <-
    obtainCommonConstraints (Exp.useEra @era) $
      fromEitherIOCli @(FileError TextEnvelopeError) $
        readFileTextEnvelope propFp
  case certScriptReq of
    OnDiskSimpleScript scriptFp -> do
      let sFp = unFile scriptFp
      s <-
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
    OnDiskPlutusScript
      (OnDiskPlutusScriptCliArgs scriptFp Exp.NoScriptDatumAllowed redeemerFile execUnits) -> do
        let plutusScriptFp = unFile scriptFp
        plutusScript <-
          readFilePlutusScript plutusScriptFp
        redeemer <-
          fromExceptTCli $
            readScriptDataOrFile redeemerFile
        case plutusScript of
          AnyPlutusScript lang script -> do
            let pScript = PScript script
            sLangSupported <-
              fromMaybeCli
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
    SimpleReferenceScript (SimpleRefScriptArgs refTxIn NoPolicyId) ->
      return
        ( proposal
        , Just . ProposalScriptWitness $
            SimpleScriptWitness
              (sbeToSimpleScriptLanguageInEra $ convert Exp.useEra)
              (SReferenceScript refTxIn)
        )
    PlutusReferenceScript
      ( PlutusRefScriptCliArgs
          refTxIn
          anyPlutusScriptVersion
          Exp.NoScriptDatumAllowed
          NoPolicyId
          redeemerFile
          execUnits
        ) -> do
        case anyPlutusScriptVersion of
          AnyPlutusScriptVersion lang -> do
            let pScript = PReferenceScript refTxIn
            redeemer <-
              fromExceptTCli $
                readScriptDataOrFile redeemerFile
            sLangSupported <-
              fromMaybeCli
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

data ProposalError
  = ProposalErrorFile (FileError CliScriptWitnessError)
  deriving Show

instance Error ProposalError where
  prettyError = pshow

readProposal
  :: Exp.IsEra era
  => (ProposalFile In, Maybe (ScriptRequirements Exp.ProposalItem))
  -> CIO e (Proposal era, Maybe (ProposalScriptWitness era))
readProposal (fp, mScriptWit) = do
  readProposalScriptWitness (fp, mScriptWit)

readTxGovernanceActions
  :: Exp.IsEra era
  => [(ProposalFile In, Maybe (ScriptRequirements Exp.ProposalItem))]
  -> CIO e [(Proposal era, Maybe (ProposalScriptWitness era))]
readTxGovernanceActions = mapM readProposal
