{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Cardano.CLI.Compatible.Transaction.Run
  ( CompatibleTransactionError (..)
  , runCompatibleTransactionCmd
  )
where

import Cardano.Api
import Cardano.Api.Compatible
import Cardano.Api.Shelley hiding (VotingProcedures)

import Cardano.CLI.Compatible.Exception
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
import Cardano.CLI.Type.Error.TxCmdError
import Cardano.CLI.Type.TxFeature

import Data.Function
import Data.Map.Strict qualified as Map
import Data.Maybe
import GHC.Exts (toList)

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
    ) = do
    shelleyBasedEraConstraints sbe $ do
      sks <- mapM (fromEitherIOCli . readWitnessSigningData) witnesses

      allOuts <- fromEitherIOCli . runExceptT $ mapM (toTxOutInAnyEra sbe) outs

      certFilesAndMaybeScriptWits <-
        fromEitherIOCli $
          runExceptT $
            readCertificateScriptWitnesses sbe certificates

      certsAndMaybeScriptWits <-
        liftIO $
          sequenceA
            [ fmap (,cswScriptWitness <$> mSwit) $
                fromEitherIOCli $
                  readFileTextEnvelope AsCertificate $
                    File certFile
            | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
            ]

      (protocolUpdates, votes) :: (AnyProtocolUpdate era, AnyVote era) <-
        caseShelleyToBabbageOrConwayEraOnwards
          ( const $ do
              case mUpdateProposal of
                Nothing -> return (NoPParamsUpdate sbe, NoVotes)
                Just p -> do
                  pparamUpdate <- fromEitherIOCli $ runExceptT $ readUpdateProposalFile p
                  return (pparamUpdate, NoVotes)
          )
          ( \w ->
              case mProposalProcedure of
                Nothing -> return (NoPParamsUpdate sbe, NoVotes)
                Just prop -> do
                  pparamUpdate <- fromEitherIOCli $ runExceptT $ readProposalProcedureFile prop
                  votesAndWits <- fromEitherIOCli (readVotingProceduresFiles w mVotes)
                  votingProcedures <-
                    fromEitherCli $ mkTxVotingProcedures [(v, vswScriptWitness <$> mSwit) | (v, mSwit) <- votesAndWits]
                  return (pparamUpdate, VotingProcedures w votingProcedures)
          )
          sbe

      let certsRefInputs =
            [ refInput
            | (_, Just sWit) <- certsAndMaybeScriptWits
            , refInput <- maybeToList $ getScriptWitnessReferenceInput sWit
            ]

          votesRefInputs =
            [ refInput
            | VotingProcedures _ (TxVotingProcedures _ (BuildTxWith voteMap)) <- [votes]
            , sWit <- Map.elems voteMap
            , refInput <- maybeToList $ getScriptWitnessReferenceInput sWit
            ]

          proposalsRefInputs =
            [ refInput
            | ProposalProcedures _ (TxProposalProcedures proposalMap) <- [protocolUpdates]
            , BuildTxWith (Just sWit) <- map snd $ toList proposalMap
            , refInput <- maybeToList $ getScriptWitnessReferenceInput sWit
            ]

      validatedRefInputs <-
        fromEitherCli . validateTxInsReference $
          certsRefInputs <> votesRefInputs <> proposalsRefInputs
      let txCerts = mkTxCertificates sbe certsAndMaybeScriptWits

      -- this body is only for witnesses
      apiTxBody <-
        fromEitherCli $
          createTransactionBody sbe $
            defaultTxBodyContent sbe
              & setTxIns (map (,BuildTxWith (KeyWitness KeyWitnessForSpending)) ins)
              & setTxOuts allOuts
              & setTxFee (TxFeeExplicit sbe fee)
              & setTxCertificates txCerts
              & setTxInsReference validatedRefInputs

      let (sksByron, sksShelley) = partitionSomeWitnesses $ map categoriseSomeSigningWitness sks

      byronWitnesses <-
        fromEitherCli $
          mkShelleyBootstrapWitnesses sbe mNetworkId apiTxBody sksByron

      let newShelleyKeyWits = map (makeShelleyKeyWitness sbe apiTxBody) sksShelley
          allKeyWits = newShelleyKeyWits ++ byronWitnesses

      signedTx <-
        fromEitherCli $
          createCompatibleSignedTx sbe ins allOuts allKeyWits fee protocolUpdates votes txCerts

      fromEitherIOCli $
        writeTxFileTextEnvelopeCddl sbe outputFp signedTx
   where
    validateTxInsReference
      :: [TxIn]
      -> Either TxCmdError (TxInsReference era)
    validateTxInsReference [] = return TxInsReferenceNone
    validateTxInsReference allRefIns = do
      let era = toCardanoEra era
          eraMismatchError = Left $ TxCmdTxFeatureMismatch (anyCardanoEra era) TxFeatureReferenceInputs
      w <- maybe eraMismatchError Right $ forEraMaybeEon era
      pure $ TxInsReference w allRefIns

readUpdateProposalFile
  :: Featured ShelleyToBabbageEra era (Maybe UpdateProposalFile)
  -> ExceptT CompatibleTransactionError IO (AnyProtocolUpdate era)
readUpdateProposalFile (Featured sToB Nothing) =
  return $ NoPParamsUpdate $ convert sToB
readUpdateProposalFile (Featured sToB (Just updateProposalFile)) = do
  prop <- firstExceptT CompatibleFileError $ readTxUpdateProposal sToB updateProposalFile
  case prop of
    TxUpdateProposalNone -> return $ NoPParamsUpdate $ convert sToB
    TxUpdateProposal _ proposal -> return $ ProtocolUpdate sToB proposal

readProposalProcedureFile
  :: Featured ConwayEraOnwards era [(ProposalFile In, Maybe CliProposalScriptRequirements)]
  -> ExceptT CompatibleTransactionError IO (AnyProtocolUpdate era)
readProposalProcedureFile (Featured cEraOnwards []) =
  let sbe = convert cEraOnwards
   in return $ NoPParamsUpdate sbe
readProposalProcedureFile (Featured cEraOnwards proposals) = do
  props <-
    mapM
      (firstExceptT CompatibleProposalError . newExceptT . readProposal cEraOnwards)
      proposals
  return $
    conwayEraOnwardsConstraints cEraOnwards $
      ProposalProcedures cEraOnwards $
        mkTxProposalProcedures
          [(govProp, pswScriptWitness <$> mScriptWit) | (Proposal govProp, mScriptWit) <- props]
