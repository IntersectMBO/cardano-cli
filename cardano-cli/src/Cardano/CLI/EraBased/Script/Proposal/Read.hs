{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Script.Proposal.Read
  ( readProposalScriptWitness
  )
where

import Cardano.Api
import Cardano.Api.Shelley

import Cardano.CLI.EraBased.Script.Proposal.Types
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Types
import Cardano.CLI.Types.Common

readProposalScriptWitness
  :: MonadIOTransError (FileError CliScriptWitnessError) t m
  => ConwayEraOnwards era
  -> (ProposalFile In, Maybe CliProposalScriptRequirements)
  -> t m (Proposal era, Maybe (ProposalScriptWitness era))
readProposalScriptWitness w (propFp, Nothing) = do
  proposal <-
    conwayEraOnwardsConstraints w $
      modifyError (fmap TextEnvelopeError) $
        hoistIOEither $
          readFileTextEnvelope AsProposal propFp
  return (proposal, Nothing)
readProposalScriptWitness w (propFp, Just certScriptReq) = do
  let sbe = convert w
  proposal <-
    conwayEraOnwardsConstraints w $
      modifyError (fmap TextEnvelopeError) $
        hoistIOEither $
          readFileTextEnvelope AsProposal propFp
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
