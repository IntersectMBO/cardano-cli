{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.EraBased.Script.Vote.Read
  ( readVoteScriptWitness
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.EraBased.Script.Vote.Type (VoteScriptWitness (..))
import Cardano.CLI.Type.Error.PlutusScriptDecodeError
import Cardano.CLI.Type.Governance

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
        fromExceptTCli $
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
          fromExceptTCli
            ( readFilePlutusScript plutusScriptFp
                :: ExceptT (FileError PlutusScriptDecodeError) IO AnyPlutusScript
            )
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
          _
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
