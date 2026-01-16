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
import Cardano.Api.Experimental.AnyScriptWitness
import Cardano.Api.Experimental.Plutus qualified as Exp

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
  -> CIO e (Proposal era, Exp.AnyWitness (Exp.LedgerEra era))
readProposalScriptWitness (propFp, Nothing) = do
  proposal <-
    obtainCommonConstraints (Exp.useEra @era) $
      fromEitherIOCli @(FileError TextEnvelopeError) $
        readFileTextEnvelope propFp
  return (proposal, Exp.AnyKeyWitnessPlaceholder)
readProposalScriptWitness (propFp, Just certScriptReq) =
  do
    proposal <-
      obtainCommonConstraints (Exp.useEra @era) $
        fromEitherIOCli @(FileError TextEnvelopeError) $
          readFileTextEnvelope propFp
    case certScriptReq of
      OnDiskSimpleScript scriptFp -> do
        let sFp = unFile scriptFp
        s <-
          Exp.AnySimpleScriptWitness . Exp.SScript <$> readFileSimpleScript sFp (Exp.useEra @era)

        return
          ( proposal
          , s
          )
      OnDiskPlutusScript
        (OnDiskPlutusScriptCliArgs scriptFp Exp.NoScriptDatumAllowed redeemerFile execUnits) -> do
          let plutusScriptFp = unFile scriptFp
          Exp.AnyPlutusScript plutusScript <-
            readFilePlutusScript @_ @era plutusScriptFp
          let lang = Exp.plutusScriptInEraSLanguage plutusScript
          redeemer <-
            fromExceptTCli $
              readScriptDataOrFile redeemerFile

          let pScript = Exp.PScript plutusScript
              sw =
                Exp.PlutusScriptWitness
                  lang
                  pScript
                  Exp.NoScriptDatum
                  redeemer
                  execUnits
          return
            ( proposal
            , Exp.AnyPlutusScriptWitness $
                AnyPlutusProposingScriptWitness sw
            )
      SimpleReferenceScript (SimpleRefScriptArgs refTxIn NoPolicyId) ->
        return
          ( proposal
          , Exp.AnySimpleScriptWitness $ Exp.SReferenceScript refTxIn
          )
      PlutusReferenceScript
        ( PlutusRefScriptCliArgs
            refTxIn
            (AnySLanguage lang)
            Exp.NoScriptDatumAllowed
            NoPolicyId
            redeemerFile
            execUnits
          ) -> do
          let pScript = Exp.PReferenceScript refTxIn
          redeemer <-
            fromExceptTCli $
              readScriptDataOrFile redeemerFile

          return
            ( proposal
            , Exp.AnyPlutusScriptWitness $
                AnyPlutusProposingScriptWitness
                  ( Exp.PlutusScriptWitness
                      lang
                      pScript
                      Exp.NoScriptDatum
                      redeemer
                      execUnits
                  )
            )

newtype ProposalError
  = ProposalErrorFile (FileError CliScriptWitnessError)
  deriving Show

instance Error ProposalError where
  prettyError = pshow

readProposal
  :: Exp.IsEra era
  => (ProposalFile In, Maybe (ScriptRequirements Exp.ProposalItem))
  -> CIO e (Proposal era, Exp.AnyWitness (Exp.LedgerEra era))
readProposal (fp, mScriptWit) = do
  readProposalScriptWitness (fp, mScriptWit)

readTxGovernanceActions
  :: Exp.IsEra era
  => [(ProposalFile In, Maybe (ScriptRequirements Exp.ProposalItem))]
  -> CIO e [(Proposal era, Exp.AnyWitness (Exp.LedgerEra era))]
readTxGovernanceActions = mapM readProposal
