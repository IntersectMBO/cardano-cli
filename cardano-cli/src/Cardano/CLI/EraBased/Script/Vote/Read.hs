{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.EraBased.Script.Vote.Read
  ( readVotingProceduresFiles
  , readVoteScriptWitness
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.EraBased.Script.Type qualified as Exp
import Cardano.CLI.EraBased.Script.Vote.Type (VoteScriptWitness (..))
import Cardano.CLI.Read
import Cardano.CLI.Type.Governance

import Control.Monad

readVoteScriptWitness
  :: ConwayEraOnwards era
  -> (VoteFile In, Maybe (ScriptRequirements Exp.VoterItem))
  -> CIO e (VotingProcedures era, Maybe (VoteScriptWitness era))
readVoteScriptWitness w (voteFp, Nothing) = do
  votProceds <-
    conwayEraOnwardsConstraints w $
      fromEitherIOCli $
        readFileTextEnvelope voteFp
  return (votProceds, Nothing)
readVoteScriptWitness w (voteFp, Just certScriptReq) = do
  let sbe = convert w
  votProceds <-
    conwayEraOnwardsConstraints w $
      fromEitherIOCli $
        readFileTextEnvelope voteFp
  case certScriptReq of
    OnDiskSimpleScript scriptFp -> do
      let sFp = unFile scriptFp
      s <-
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
    PlutusReferenceScript
      ( PlutusRefScriptCliArgs
          refTxIn
          anyPlutusScriptVersion
          Exp.NoScriptDatumAllowed
          Exp.NoPolicyId
          redeemerFile
          execUnits
        ) -> do
        case anyPlutusScriptVersion of
          AnyPlutusScriptVersion lang -> do
            let pScript = PReferenceScript refTxIn
            redeemer <-
              fromExceptTCli $ readScriptDataOrFile redeemerFile
            sLangSupported <-
              fromMaybeCli
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
    SimpleReferenceScript (SimpleRefScriptArgs refTxIn _) ->
      return
        ( votProceds
        , Just $
            VoteScriptWitness $
              SimpleScriptWitness
                (sbeToSimpleScriptLanguageInEra sbe)
                (SReferenceScript refTxIn)
        )

-- Because the 'Voter' type is contained only in the 'VotingProcedures'
-- type, we must read a single vote as 'VotingProcedures'. The cli will
-- not read vote files with multiple votes in them because this will
-- complicate the code further in terms of contructing the redeemer map
-- when it comes to script witnessed votes.
readVotingProceduresFiles
  :: ConwayEraOnwards era
  -> [(VoteFile In, Maybe (ScriptRequirements Exp.VoterItem))]
  -> CIO e [(VotingProcedures era, Maybe (VoteScriptWitness era))]
readVotingProceduresFiles w files =
  forM files (readVoteScriptWitness w)
