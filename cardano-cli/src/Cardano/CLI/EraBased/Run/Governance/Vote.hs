{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Run.Governance.Vote
  ( runGovernanceVoteCmds
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley

import qualified Cardano.CLI.EraBased.Commands.Governance.Vote as Cmd
import           Cardano.CLI.EraBased.Script.Vote.Read
import           Cardano.CLI.Run.Hash (carryHashChecks)
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.CmdError
import           Cardano.CLI.Types.Errors.GovernanceVoteCmdError
import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Key

import           Data.Aeson.Encode.Pretty
import           Data.Function
import qualified Data.Yaml.Pretty as Yaml

runGovernanceVoteCmds
  :: ()
  => Cmd.GovernanceVoteCmds era
  -> ExceptT CmdError IO ()
runGovernanceVoteCmds = \case
  Cmd.GovernanceVoteCreateCmd args ->
    runGovernanceVoteCreateCmd args
      & firstExceptT CmdGovernanceVoteError
  Cmd.GovernanceVoteViewCmd args ->
    runGovernanceVoteViewCmd args
      & firstExceptT CmdGovernanceVoteError

runGovernanceVoteCreateCmd
  :: forall era
   . ()
  => Cmd.GovernanceVoteCreateCmdArgs era
  -> ExceptT GovernanceVoteCmdError IO ()
runGovernanceVoteCreateCmd
  Cmd.GovernanceVoteCreateCmdArgs
    { eon
    , voteChoice
    , governanceAction
    , votingStakeCredentialSource
    , mAnchor
    , outFile
    } = do
    let (govActionTxId, govActionIndex) = governanceAction
        sbe = convert eon -- TODO: Conway era - update vote creation related function to take ConwayEraOnwards
        mAnchor' =
          fmap
            ( \pca@PotentiallyCheckedAnchor{pcaAnchor = (VoteUrl url, voteHash)} ->
                pca{pcaAnchor = L.Anchor{L.anchorUrl = url, L.anchorDataHash = voteHash}}
            )
            mAnchor

    mapM_
      ( withExceptT GovernanceVoteCmdResignationCertHashCheckError
          . carryHashChecks (validateGovActionAnchorData CIP108)
      )
      mAnchor'

    voteProcedure <- case mAnchor' of
      Nothing -> pure $ createVotingProcedure eon voteChoice Nothing
      Just voteAnchor ->
        shelleyBasedEraConstraints sbe $
          let VotingProcedure votingProcedureWithoutAnchor = createVotingProcedure eon voteChoice Nothing
              votingProcedureWithAnchor = VotingProcedure $ votingProcedureWithoutAnchor{L.vProcAnchor = L.SJust (pcaAnchor voteAnchor)}
           in return votingProcedureWithAnchor

    shelleyBasedEraConstraints sbe $ do
      voter <- firstExceptT GovernanceVoteCmdReadVerificationKeyError $ case votingStakeCredentialSource of
        AnyDRepVerificationKeyOrHashOrFileOrScriptHash stake -> do
          drepCred <- readVerificationKeyOrHashOrFileOrScriptHash AsDRepKey unDRepKeyHash stake
          pure $ L.DRepVoter drepCred
        AnyStakePoolVerificationKeyOrHashOrFile stake -> do
          StakePoolKeyHash h <- readVerificationKeyOrHashOrTextEnvFile AsStakePoolKey stake
          pure $ L.StakePoolVoter h
        AnyCommitteeHotVerificationKeyOrHashOrFileOrScriptHash stake -> do
          hotCred <- readVerificationKeyOrHashOrFileOrScriptHash AsCommitteeHotKey unCommitteeHotKeyHash stake
          pure $ L.CommitteeVoter hotCred

      let govActIdentifier = createGovernanceActionId govActionTxId govActionIndex
          votingProcedures = singletonVotingProcedures eon voter govActIdentifier (unVotingProcedure voteProcedure)
      firstExceptT GovernanceVoteCmdWriteError . newExceptT $
        writeFileTextEnvelope outFile Nothing votingProcedures

runGovernanceVoteViewCmd
  :: forall era
   . ()
  => Cmd.GovernanceVoteViewCmdArgs era
  -> ExceptT GovernanceVoteCmdError IO ()
runGovernanceVoteViewCmd
  Cmd.GovernanceVoteViewCmdArgs
    { eon
    , outFormat
    , voteFile
    , mOutFile
    } = do
    let sbe :: ShelleyBasedEra era = convert eon

    shelleyBasedEraConstraints sbe $ do
      voteProcedures <-
        fmap fst $
          firstExceptT GovernanceVoteCmdReadVoteFileError $
            readVoteScriptWitness eon (voteFile, Nothing)
      firstExceptT GovernanceVoteCmdWriteError
        . newExceptT
        . ( case outFormat of
              ViewOutputFormatYaml ->
                writeByteStringOutput mOutFile
                  . Yaml.encodePretty
                    (Yaml.setConfCompare compare Yaml.defConfig)
              ViewOutputFormatJson ->
                writeLazyByteStringOutput mOutFile
                  . encodePretty'
                    (defConfig{confCompare = compare})
          )
        . unVotingProcedures
        $ voteProcedures
