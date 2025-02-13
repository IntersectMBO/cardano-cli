{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Cardano.CLI.Compatible.Transaction.Run
  ( runCompatibleTransactionCmd
  , CompatibleTransactionError (..)
  )
where

import           Cardano.Api
import           Cardano.Api.Compatible as Compatible
import           Cardano.Api.Shelley hiding (VotingProcedures)

import           Cardano.CLI.Compatible.Transaction.Commands
import           Cardano.CLI.EraBased.Run.Transaction
import           Cardano.CLI.EraBased.Script.Certificate.Read
import           Cardano.CLI.EraBased.Script.Certificate.Types
import           Cardano.CLI.EraBased.Script.Proposal.Types
import           Cardano.CLI.EraBased.Script.Types
import           Cardano.CLI.EraBased.Script.Vote.Types (VoteScriptWitness (..))
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.BootstrapWitnessError
import           Cardano.CLI.Types.Errors.TxCmdError
import           Cardano.CLI.Types.TxFeature

import           Data.Bifunctor (first)
import           Data.Function
import qualified Data.Map.Strict as Map
import           Data.Maybe

runCompatibleTransactionCmd
  :: forall era
   . CompatibleTransactionCmds era
  -> ExceptT CompatibleTransactionError IO ()
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
    sks <- firstExceptT CompatibleWitnessError $ mapM (newExceptT . readWitnessSigningData) witnesses

    allOuts <- firstExceptT CompatibleTxCmdError $ mapM (toTxOutInAnyEra sbe) outs

    certFilesAndMaybeScriptWits <-
      firstExceptT CompatibleScriptWitnessReadError $
        readCertificateScriptWitnesses sbe certificates

    certsAndMaybeScriptWits :: [(Certificate era, Maybe (ScriptWitness WitCtxStake era))] <-
      shelleyBasedEraConstraints sbe $
        sequence
          [ fmap
              (,cswScriptWitness <$> mSwit)
              ( firstExceptT CompatibleFileError . newExceptT $
                  readFileTextEnvelope AsCertificate (File certFile)
              )
          | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
          ]

    (protocolUpdates, votes) :: (AnyProtocolUpdate era, AnyVote era) <-
      caseShelleyToBabbageOrConwayEraOnwards
        ( const $ do
            prop <- maybe (pure $ NoPParamsUpdate sbe) readUpdateProposalFile mUpdateProposal
            return (prop, NoVotes)
        )
        ( \w -> do
            prop <- maybe (pure $ NoPParamsUpdate sbe) readProposalProcedureFile mProposalProcedure
            votesAndWits <-
              firstExceptT CompatibleVoteError . newExceptT $
                readVotingProceduresFiles w mVotes
            votingProcedures <-
              firstExceptT CompatibleVoteMergeError . hoistEither $
                mkTxVotingProcedures [(v, vswScriptWitness <$> mSwit) | (v, mSwit) <- votesAndWits]
            return (prop, VotingProcedures w votingProcedures)
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
          | ProposalProcedures _ (TxProposalProcedures _ (BuildTxWith proposalMap)) <- [protocolUpdates]
          , sWit <- Map.elems proposalMap
          , refInput <- maybeToList $ getScriptWitnessReferenceInput sWit
          ]

    validatedRefInputs <-
      liftEither . first CompatibleTxCmdError . validateTxInsReference $
        certsRefInputs <> votesRefInputs <> proposalsRefInputs
    let txCerts = mkTxCertificates sbe certsAndMaybeScriptWits

    -- this body is only for witnesses
    apiTxBody <-
      firstExceptT CompatibleTxBodyError $
        hoistEither $
          createTransactionBody sbe $
            defaultTxBodyContent sbe
              & setTxIns (map (,BuildTxWith (KeyWitness KeyWitnessForSpending)) ins)
              & setTxOuts allOuts
              & setTxFee (TxFeeExplicit sbe fee)
              & setTxCertificates txCerts
              & setTxInsReference validatedRefInputs

    let (sksByron, sksShelley) = partitionSomeWitnesses $ map categoriseSomeSigningWitness sks

    byronWitnesses <-
      firstExceptT CompatibleBootstrapWitnessError . hoistEither $
        mkShelleyBootstrapWitnesses sbe mNetworkId apiTxBody sksByron

    let newShelleyKeyWits = map (makeShelleyKeyWitness sbe apiTxBody) sksShelley
        allKeyWits = newShelleyKeyWits ++ byronWitnesses

    signedTx <-
      firstExceptT CompatiblePParamsConversionError . hoistEither $
        createCompatibleSignedTx sbe ins allOuts allKeyWits fee protocolUpdates votes txCerts

    firstExceptT CompatibleFileError $
      newExceptT $
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

data CompatibleTransactionError
  = CompatibleTxCmdError !TxCmdError
  | CompatibleWitnessError !ReadWitnessSigningDataError
  | CompatiblePParamsConversionError !ProtocolParametersConversionError
  | CompatibleBootstrapWitnessError !BootstrapWitnessError
  | forall err. Error err => CompatibleFileError (FileError err)
  | CompatibleTxBodyError !TxBodyError
  | CompatibleProposalError !ProposalError
  | CompatibleVoteError !VoteError
  | forall era. CompatibleVoteMergeError !(VotesMergingConflict era)
  | CompatibleScriptWitnessError !ScriptWitnessError
  | CompatibleScriptWitnessReadError !(FileError CliScriptWitnessError)

instance Error CompatibleTransactionError where
  prettyError = \case
    CompatibleTxCmdError e -> renderTxCmdError e
    CompatibleWitnessError e -> renderReadWitnessSigningDataError e
    CompatiblePParamsConversionError e -> prettyError e
    CompatibleBootstrapWitnessError e -> renderBootstrapWitnessError e
    CompatibleFileError e -> prettyError e
    CompatibleTxBodyError e -> prettyError e
    CompatibleProposalError e -> pshow e
    CompatibleVoteError e -> pshow e
    CompatibleVoteMergeError e -> pshow e
    CompatibleScriptWitnessError e -> renderScriptWitnessError e
    CompatibleScriptWitnessReadError e -> prettyError e
