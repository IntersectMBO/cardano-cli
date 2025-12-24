{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Cardano.CLI.Compatible.Transaction.Run
  ( runCompatibleTransactionCmd
  )
where

import Cardano.Api hiding (VotingProcedures)
import Cardano.Api qualified as OldApi
import Cardano.Api.Compatible
import Cardano.Api.Experimental (obtainCommonConstraints)
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Ledger qualified as L hiding
  ( VotingProcedures
  )

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Compatible.Transaction.Command
import Cardano.CLI.Compatible.Transaction.ScriptWitness
import Cardano.CLI.Compatible.Transaction.TxOut
import Cardano.CLI.EraBased.Script.Certificate.Type
import Cardano.CLI.EraBased.Script.Proposal.Read
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.EraBased.Script.Vote.Read
import Cardano.CLI.EraBased.Transaction.Run
import Cardano.CLI.Read
import Cardano.CLI.Type.Common

import Control.Monad
import Data.Typeable
import Lens.Micro

runCompatibleTransactionCmd
  :: forall era e
   . CompatibleTransactionCmds era
  -> CIO e ()
runCompatibleTransactionCmd
  ( CreateCompatibleSignedTransaction
      sbe
      ins
      outs
      mUpdateProposal
      mProposalProcedure
      mVotes
      witnesses
      mNetworkId
      fee
      certificates
      outputFp
    ) = shelleyBasedEraConstraints sbe $ do
    sks <- mapM (fromEitherIOCli . readWitnessSigningData) witnesses

    allOuts <- mapM (toTxOutInAnyEra sbe) outs

    certFilesAndMaybeScriptWits <-
      readCertificateScriptWitnesses sbe certificates

    certsAndMaybeScriptWits <-
      liftIO $
        sequenceA
          [ fmap (,cswScriptWitness <$> mSwit) $
              fromEitherIOCli $
                readFileTextEnvelope $
                  File certFile
          | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
          ]

    (protocolUpdates, votes) :: (AnyProtocolUpdate era, AnyVote era) <-
      caseShelleyToBabbageOrConwayEraOnwards
        ( const $ do
            case mUpdateProposal of
              Nothing -> return (NoPParamsUpdate sbe, NoVotes)
              Just p -> do
                pparamUpdate <- readUpdateProposalFile p
                return (pparamUpdate, NoVotes)
        )
        ( \w ->
            case mProposalProcedure of
              Nothing -> return (NoPParamsUpdate sbe, NoVotes)
              Just prop -> do
                pparamUpdate <- readProposalProcedureFile prop
                votesAndWits :: [(OldApi.VotingProcedures era, Exp.AnyWitness (Exp.LedgerEra era))] <-
                  obtainCommonConstraints (convert w) $ readVotingProceduresFiles mVotes
                votingProcedures :: (Exp.TxVotingProcedures (Exp.LedgerEra era)) <-
                  obtainTypeable w $
                    fromEitherCli
                      ( Exp.mkTxVotingProcedures
                          [ (obtainCommonConstraints (convert w) $ OldApi.unVotingProcedures vp, anyW)
                          | (vp, anyW) <- votesAndWits
                          ]
                      )
                return (pparamUpdate, VotingProcedures w $ obtainCommonConstraints (convert w) votingProcedures)
        )
        sbe

    let txCerts = mkTxCertificates sbe certsAndMaybeScriptWits

    transaction@(ShelleyTx _ ledgerTx) <-
      fromEitherCli $
        createCompatibleTx sbe ins allOuts fee protocolUpdates votes txCerts

    let txBody = ledgerTx ^. L.bodyTxL

    let (sksByron, sksShelley) = partitionSomeWitnesses $ map categoriseSomeSigningWitness sks

    byronWitnesses <-
      forM sksByron $
        fromEitherCli
          . mkShelleyBootstrapWitness sbe mNetworkId txBody

    let newShelleyKeyWits = makeShelleyKeyWitness' sbe txBody <$> sksShelley
        allKeyWits = newShelleyKeyWits ++ byronWitnesses
        signedTx = addWitnesses allKeyWits transaction

    fromEitherIOCli $
      writeTxFileTextEnvelope sbe outputFp signedTx

obtainTypeable
  :: ConwayEraOnwards era
  -> (Typeable (Exp.LedgerEra era) => r)
  -> r
obtainTypeable ConwayEraOnwardsConway r = r
obtainTypeable ConwayEraOnwardsDijkstra r = r

readUpdateProposalFile
  :: Featured ShelleyToBabbageEra era (Maybe UpdateProposalFile)
  -> CIO e (AnyProtocolUpdate era)
readUpdateProposalFile (Featured sToB Nothing) =
  return $ NoPParamsUpdate $ convert sToB
readUpdateProposalFile (Featured sToB (Just updateProposalFile)) = do
  prop <-
    fromExceptTCli $ readTxUpdateProposal sToB updateProposalFile
  case prop of
    TxUpdateProposalNone -> return $ NoPParamsUpdate $ convert sToB
    TxUpdateProposal _ proposal -> return $ ProtocolUpdate sToB proposal

readProposalProcedureFile
  :: forall e era
   . Featured ConwayEraOnwards era [(ProposalFile In, Maybe (ScriptRequirements Exp.ProposalItem))]
  -> CIO e (AnyProtocolUpdate era)
readProposalProcedureFile (Featured cEraOnwards []) =
  let sbe = convert cEraOnwards
   in return $ NoPParamsUpdate sbe
readProposalProcedureFile (Featured cEraOnwards proposals) = do
  let era = convert cEraOnwards
  props :: [(Proposal era, Exp.AnyWitness (Exp.LedgerEra era))] <-
    Exp.obtainCommonConstraints era $ mapM readProposal proposals

  return $
    Exp.obtainCommonConstraints era $
      ProposalProcedures cEraOnwards $
        mkTxProposalProcedures
          [(govProp, conv swit) | (Proposal govProp, swit) <- props]

conv :: Exp.AnyWitness (Exp.LedgerEra era) -> Maybe (ScriptWitness WitCtxStake era)
conv Exp.AnyKeyWitnessPlaceholder = Nothing
conv _ = Nothing
