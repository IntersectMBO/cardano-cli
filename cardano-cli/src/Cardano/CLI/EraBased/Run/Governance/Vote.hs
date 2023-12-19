{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Run.Governance.Vote
  ( runGovernanceVoteCmds
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import qualified Cardano.CLI.EraBased.Commands.Governance.Vote as Cmd
import           Cardano.CLI.Read (readVotingProceduresFile)
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.CmdError
import           Cardano.CLI.Types.Errors.GovernanceVoteCmdError
import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Key

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Data.Aeson.Encode.Pretty
import           Data.Function
import qualified Data.Yaml.Pretty as Yaml

runGovernanceVoteCmds :: ()
  => Cmd.GovernanceVoteCmds era
  -> ExceptT CmdError IO ()
runGovernanceVoteCmds = \case
  Cmd.GovernanceVoteCreateCmd args ->
    runGovernanceVoteCreateCmd args
      & firstExceptT CmdGovernanceVoteError
  Cmd.GovernanceVoteViewCmd args ->
    runGovernanceVoteViewCmd args
      & firstExceptT CmdGovernanceVoteError

runGovernanceVoteCreateCmd :: ()
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
  let sbe = conwayEraOnwardsToShelleyBasedEra eon -- TODO: Conway era - update vote creation related function to take ConwayEraOnwards
  voteProcedure <- case mAnchor of
     Nothing -> pure $ createVotingProcedure eon voteChoice Nothing
     Just (VoteUrl url, voteHash) -> shelleyBasedEraConstraints sbe $ do
       let voteAnchor = Ledger.Anchor { Ledger.anchorUrl = url, Ledger.anchorDataHash = voteHash }
           VotingProcedure votingProcedureWithoutAnchor = createVotingProcedure eon voteChoice Nothing
           votingProcedureWithAnchor = VotingProcedure $ votingProcedureWithoutAnchor { Ledger.vProcAnchor = Ledger.SJust voteAnchor }
       pure votingProcedureWithAnchor

  shelleyBasedEraConstraints sbe $ do
    voter <- firstExceptT  GovernanceVoteCmdReadVerificationKeyError $ case votingStakeCredentialSource of
      AnyDRepVerificationKeyOrHashOrFile stake -> do
        DRepKeyHash h <- newExceptT $ readVerificationKeyOrHashOrTextEnvFile AsDRepKey stake
        pure $ Ledger.DRepVoter $ Ledger.KeyHashObj h

      AnyStakePoolVerificationKeyOrHashOrFile stake -> do
        StakePoolKeyHash h <- newExceptT $ readVerificationKeyOrHashOrTextEnvFile AsStakePoolKey stake
        pure $ Ledger.StakePoolVoter h

      AnyCommitteeHotVerificationKeyOrHashOrFile stake -> do
        CommitteeHotKeyHash h <-  newExceptT $ readVerificationKeyOrHashOrTextEnvFile AsCommitteeHotKey stake
        pure $ Ledger.CommitteeVoter $ Ledger.KeyHashObj h

    let govActIdentifier = createGovernanceActionId govActionTxId govActionIndex
        votingProcedures = singletonVotingProcedures eon voter govActIdentifier (unVotingProcedure voteProcedure)
    firstExceptT GovernanceVoteCmdWriteError . newExceptT $ writeFileTextEnvelope outFile Nothing votingProcedures

runGovernanceVoteViewCmd :: ()
  => Cmd.GovernanceVoteViewCmdArgs era
  -> ExceptT GovernanceVoteCmdError IO ()
runGovernanceVoteViewCmd
    Cmd.GovernanceVoteViewCmdArgs
      { eon
      , outFormat
      , voteFile
      , mOutFile
      } = do
  let sbe = conwayEraOnwardsToShelleyBasedEra eon

  shelleyBasedEraConstraints sbe $ do
    voteProcedures <- firstExceptT GovernanceVoteCmdReadVoteFileError . newExceptT $
     readVotingProceduresFile eon voteFile
    firstExceptT GovernanceVoteCmdWriteError .
      newExceptT .
      (case outFormat of
         ViewOutputFormatYaml ->
           writeByteStringOutput mOutFile . Yaml.encodePretty
             (Yaml.setConfCompare compare Yaml.defConfig)
         ViewOutputFormatJson ->
           writeLazyByteStringOutput mOutFile . encodePretty'
             (defConfig {confCompare = compare})) .
      unVotingProcedures $
      voteProcedures