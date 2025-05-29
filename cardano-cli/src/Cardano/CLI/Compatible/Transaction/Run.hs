{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Compatible.Transaction.Run
  ( runCompatibleTransactionCmd
  )
where

import Cardano.Api
import Cardano.Api.Compatible
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley hiding (VotingProcedures)

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Compatible.Helper
import Cardano.CLI.Compatible.Transaction.Command
import Cardano.CLI.EraBased.Script.Certificate.Read
import Cardano.CLI.EraBased.Script.Certificate.Type
import Cardano.CLI.EraBased.Script.Proposal.Type
import Cardano.CLI.EraBased.Script.Vote.Type
  ( VoteScriptWitness (..)
  )
import Cardano.CLI.EraBased.Transaction.Run
import Cardano.CLI.Read
import Cardano.CLI.Type.Common

import Control.Monad
import Lens.Micro

data CompatibleTransactionError
  = forall err. Error err => CompatibleFileError (FileError err)
  | CompatibleProposalError !ProposalError

instance Show CompatibleTransactionError where
  show = show . prettyError

instance Error CompatibleTransactionError where
  prettyError = \case
    CompatibleFileError e -> prettyError e
    CompatibleProposalError e -> pshow e

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
      fromExceptTCli $
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
                votesAndWits <- fromEitherIOCli (readVotingProceduresFiles w mVotes)
                votingProcedures <-
                  fromEitherCli $ mkTxVotingProcedures [(v, vswScriptWitness <$> mSwit) | (v, mSwit) <- votesAndWits]
                return (pparamUpdate, VotingProcedures w votingProcedures)
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
      writeTxFileTextEnvelopeCddl sbe outputFp signedTx

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
   . Featured ConwayEraOnwards era [(ProposalFile In, Maybe CliProposalScriptRequirements)]
  -> CIO e (AnyProtocolUpdate era)
readProposalProcedureFile (Featured cEraOnwards []) =
  let sbe = convert cEraOnwards
   in return $ NoPParamsUpdate sbe
readProposalProcedureFile (Featured cEraOnwards proposals) = do
  let era = convert cEraOnwards
  props :: [(Proposal era, Maybe (ProposalScriptWitness era))] <-
    mapM
      ( \p ->
          fromEitherIOCli @ProposalError $ Exp.obtainCommonConstraints era (readProposal p)
      )
      proposals

  return $
    Exp.obtainCommonConstraints era $
      ProposalProcedures cEraOnwards $
        mkTxProposalProcedures
          [(govProp, pswScriptWitness <$> mScriptWit) | (Proposal govProp, mScriptWit) <- props]
