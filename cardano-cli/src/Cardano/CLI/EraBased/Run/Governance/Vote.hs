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
import           Cardano.CLI.Read (readVoteHashSource, readVotingProceduresFile)
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.CmdError
import           Cardano.CLI.Types.Errors.GovernanceVoteCmdError
import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Key
import           Cardano.Ledger.Keys (coerceKeyRole)

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Data.Aeson.Encode.Pretty
import           Data.Bifunctor
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
     Just (VoteUrl url, voteHashSource) -> shelleyBasedEraConstraints sbe $ do
       voteHash <- firstExceptT GovernanceVoteCmdReadVoteTextError $ readVoteHashSource voteHashSource
       let voteAnchor = Ledger.Anchor { Ledger.anchorUrl = url, Ledger.anchorDataHash = voteHash }
           VotingProcedure votingProcedureWithoutAnchor = createVotingProcedure eon voteChoice Nothing
           votingProcedureWithAnchor = VotingProcedure $ votingProcedureWithoutAnchor { Ledger.vProcAnchor = Ledger.SJust voteAnchor }
       return votingProcedureWithAnchor

  shelleyBasedEraConstraints sbe $ do
    case votingStakeCredentialSource of
      AnyDRepVerificationKeyOrHashOrFile stake -> do
        DRepKeyHash h <- firstExceptT  GovernanceVoteCmdReadVerificationKeyError
                                . newExceptT $ readVerificationKeyOrHashOrTextEnvFile AsDRepKey stake
        let vStakeCred = StakeCredentialByKey . StakeKeyHash $ coerceKeyRole h

        votingCred <- hoistEither $ first GovernanceVoteCmdCredentialDecodeError $ toVotingCredential eon vStakeCred
        let voter = Ledger.DRepVoter (unVotingCredential votingCred)
            govActIdentifier = createGovernanceActionId govActionTxId govActionIndex
            votingProcedures = singletonVotingProcedures eon voter govActIdentifier (unVotingProcedure voteProcedure)
        firstExceptT GovernanceVoteCmdWriteError . newExceptT $ writeFileTextEnvelope outFile Nothing votingProcedures

      AnyStakePoolVerificationKeyOrHashOrFile stake -> do
        h <- firstExceptT GovernanceVoteCmdReadVerificationKeyError
              . newExceptT $ readVerificationKeyOrHashOrTextEnvFile AsStakePoolKey stake

        let voter = Ledger.StakePoolVoter (unStakePoolKeyHash h)
            govActIdentifier = createGovernanceActionId govActionTxId govActionIndex
            votingProcedures = singletonVotingProcedures eon voter govActIdentifier (unVotingProcedure voteProcedure)
        firstExceptT GovernanceVoteCmdWriteError . newExceptT $ writeFileTextEnvelope outFile Nothing votingProcedures

      AnyCommitteeHotVerificationKeyOrHashOrFile stake -> do
        CommitteeHotKeyHash h <- firstExceptT GovernanceVoteCmdReadVerificationKeyError
                                  . newExceptT $ readVerificationKeyOrHashOrTextEnvFile AsCommitteeHotKey stake
        let vStakeCred = StakeCredentialByKey . StakeKeyHash $ coerceKeyRole h
        votingCred <- hoistEither $ first GovernanceVoteCmdCredentialDecodeError $ toVotingCredential eon vStakeCred
        let voter = Ledger.CommitteeVoter (Ledger.coerceKeyRole (unVotingCredential votingCred)) -- TODO Conway - remove coerceKeyRole
            govActIdentifier = createGovernanceActionId govActionTxId govActionIndex
            votingProcedures = singletonVotingProcedures eon voter govActIdentifier (unVotingProcedure voteProcedure)
        firstExceptT GovernanceVoteCmdWriteError . newExceptT $ writeFileTextEnvelope outFile Nothing votingProcedures

runGovernanceVoteViewCmd :: ()
  => Cmd.GovernanceVoteViewCmdArgs era
  -> ExceptT GovernanceVoteCmdError IO ()
runGovernanceVoteViewCmd
    Cmd.GovernanceVoteViewCmdArgs
      { eon
      , yamlOutput
      , voteFile
      , mOutFile
      } = do
  let sbe = conwayEraOnwardsToShelleyBasedEra eon

  shelleyBasedEraConstraints sbe $ do
    voteProcedures <- firstExceptT GovernanceVoteCmdReadVoteFileError . newExceptT $
     readVotingProceduresFile eon voteFile
    firstExceptT GovernanceVoteCmdWriteError .
      newExceptT .
      (if yamlOutput
      then writeByteStringOutput mOutFile . Yaml.encodePretty (Yaml.setConfCompare compare Yaml.defConfig)
      else writeLazyByteStringOutput mOutFile . encodePretty' (defConfig {confCompare = compare})) .
      unVotingProcedures $
      voteProcedures
