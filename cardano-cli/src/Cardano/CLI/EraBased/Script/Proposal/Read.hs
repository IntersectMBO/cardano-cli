{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Cardano.Api.Experimental.Plutus qualified as Exp.Plutus

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Read
import Cardano.CLI.Type.Common

readProposalScriptWitness
  :: forall e era
   . Exp.IsEra era
  => (ProposalFile In, Maybe AnyNonAssetScript)
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
      AnyNonAssetScriptSimple simpleReq ->
        case simpleReq of
          OnDiskSimpleScript scriptFp -> do
            let sFp = unFile scriptFp
            s <-
              Exp.AnySimpleScriptWitness . Exp.SScript <$> readFileSimpleScript sFp (Exp.useEra @era)
            return (proposal, s)
          ReferenceSimpleScript refTxIn ->
            return
              ( proposal
              , Exp.AnySimpleScriptWitness $ Exp.SReferenceScript refTxIn
              )
      AnyNonAssetScriptPlutus plutusReq ->
        case plutusReq of
          OnDiskPlutusNonAssetScript scriptFp redeemerFile execUnits -> do
            let plutusScriptFp = unFile scriptFp
            Exp.Plutus.AnyPlutusScript plutusScript <-
              readFilePlutusScript @_ @era plutusScriptFp
            let lang = Exp.Plutus.plutusScriptInEraSLanguage plutusScript
            redeemer <-
              fromExceptTCli $
                readScriptDataOrFile redeemerFile

            let pScript = Exp.Plutus.PScript plutusScript
                sw =
                  Exp.Plutus.PlutusScriptWitness
                    lang
                    pScript
                    Exp.Plutus.NoScriptDatum
                    redeemer
                    execUnits
            return
              ( proposal
              , Exp.AnyPlutusScriptWitness $
                  AnyPlutusProposingScriptWitness sw
              )
          ReferencePlutusNonAssetScript refTxIn (AnySLanguage lang) redeemerFile execUnits -> do
            let pScript = Exp.Plutus.PReferenceScript refTxIn
            redeemer <-
              fromExceptTCli $
                readScriptDataOrFile redeemerFile

            return
              ( proposal
              , Exp.AnyPlutusScriptWitness $
                  AnyPlutusProposingScriptWitness
                    ( Exp.Plutus.PlutusScriptWitness
                        lang
                        pScript
                        Exp.Plutus.NoScriptDatum
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
  => (ProposalFile In, Maybe AnyNonAssetScript)
  -> CIO e (Proposal era, Exp.AnyWitness (Exp.LedgerEra era))
readProposal (fp, mScriptWit) = do
  readProposalScriptWitness (fp, mScriptWit)

readTxGovernanceActions
  :: Exp.IsEra era
  => [(ProposalFile In, Maybe AnyNonAssetScript)]
  -> CIO e [(Proposal era, Exp.AnyWitness (Exp.LedgerEra era))]
readTxGovernanceActions = mapM readProposal
