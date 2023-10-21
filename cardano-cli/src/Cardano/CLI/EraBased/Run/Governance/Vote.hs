{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
  Cmd.GovernanceVoteCreateCmd anyVote ->
    runGovernanceVoteCreateCmd anyVote
      & firstExceptT CmdGovernanceVoteError
  Cmd.GovernanceVoteViewCmd (Cmd.GovernanceVoteViewCmdArgs w printYaml voteFile mOutFile) ->
    runGovernanceVoteViewCmd w printYaml voteFile mOutFile
      & firstExceptT CmdGovernanceVoteError

runGovernanceVoteCreateCmd :: ()
  => Cmd.GovernanceVoteCreateCmdArgs era
  -> ExceptT GovernanceVoteCmdError IO ()
runGovernanceVoteCreateCmd (Cmd.GovernanceVoteCreateCmdArgs cOnwards voteChoice (govActionTxId, govActionIndex) voteStakeCred oFp mAnchor)  = do
  let sbe = conwayEraOnwardsToShelleyBasedEra cOnwards -- TODO: Conway era - update vote creation related function to take ConwayEraOnwards
  voteProcedure <- case mAnchor of
     Nothing -> pure $ createVotingProcedure cOnwards voteChoice Nothing
     Just (VoteUrl url, voteHashSource) -> shelleyBasedEraConstraints sbe $ do
       voteHash <- firstExceptT GovernanceVoteCmdReadVoteTextError $ readVoteHashSource voteHashSource
       let voteAnchor = Ledger.Anchor { Ledger.anchorUrl = url, Ledger.anchorDataHash = voteHash }
           VotingProcedure votingProcedureWithoutAnchor = createVotingProcedure cOnwards voteChoice Nothing
           votingProcedureWithAnchor = VotingProcedure $ votingProcedureWithoutAnchor { Ledger.vProcAnchor = Ledger.SJust voteAnchor }
       return votingProcedureWithAnchor

  shelleyBasedEraConstraints sbe $ do
    case voteStakeCred of
      AnyDRepVerificationKeyOrHashOrFile stake -> do
        DRepKeyHash h <- firstExceptT  GovernanceVoteCmdReadVerificationKeyError
                                . newExceptT $ readVerificationKeyOrHashOrTextEnvFile AsDRepKey stake
        let vStakeCred = StakeCredentialByKey . StakeKeyHash $ coerceKeyRole h

        votingCred <- hoistEither $ first GovernanceVoteCmdCredentialDecodeError $ toVotingCredential cOnwards vStakeCred
        let voter = Ledger.DRepVoter (unVotingCredential votingCred)
            govActIdentifier = createGovernanceActionId govActionTxId govActionIndex
            votingProcedures = singletonVotingProcedures cOnwards voter govActIdentifier (unVotingProcedure voteProcedure)
        firstExceptT GovernanceVoteCmdWriteError . newExceptT $ writeFileTextEnvelope oFp Nothing votingProcedures

      AnyStakePoolVerificationKeyOrHashOrFile stake -> do
        h <- firstExceptT GovernanceVoteCmdReadVerificationKeyError
              . newExceptT $ readVerificationKeyOrHashOrTextEnvFile AsStakePoolKey stake

        let voter = Ledger.StakePoolVoter (unStakePoolKeyHash h)
            govActIdentifier = createGovernanceActionId govActionTxId govActionIndex
            votingProcedures = singletonVotingProcedures cOnwards voter govActIdentifier (unVotingProcedure voteProcedure)
        firstExceptT GovernanceVoteCmdWriteError . newExceptT $ writeFileTextEnvelope oFp Nothing votingProcedures

      AnyCommitteeHotVerificationKeyOrHashOrFile stake -> do
        CommitteeHotKeyHash h <- firstExceptT GovernanceVoteCmdReadVerificationKeyError
                                  . newExceptT $ readVerificationKeyOrHashOrTextEnvFile AsCommitteeHotKey stake
        let vStakeCred = StakeCredentialByKey . StakeKeyHash $ coerceKeyRole h
        votingCred <- hoistEither $ first GovernanceVoteCmdCredentialDecodeError $ toVotingCredential cOnwards vStakeCred
        let voter = Ledger.CommitteeVoter (Ledger.coerceKeyRole (unVotingCredential votingCred)) -- TODO Conway - remove coerceKeyRole
            govActIdentifier = createGovernanceActionId govActionTxId govActionIndex
            votingProcedures = singletonVotingProcedures cOnwards voter govActIdentifier (unVotingProcedure voteProcedure)
        firstExceptT GovernanceVoteCmdWriteError . newExceptT $ writeFileTextEnvelope oFp Nothing votingProcedures

runGovernanceVoteViewCmd :: ()
  => ConwayEraOnwards era
  -> Bool
  -> VoteFile In
  -> Maybe (File () Out)
  -> ExceptT GovernanceVoteCmdError IO ()
runGovernanceVoteViewCmd w outputYaml fp mOutFile = do
  let sbe = conwayEraOnwardsToShelleyBasedEra w

  shelleyBasedEraConstraints sbe $ do
    voteProcedures <- firstExceptT GovernanceVoteCmdReadVoteFileError . newExceptT $
     readVotingProceduresFile w fp
    firstExceptT GovernanceVoteCmdWriteError .
      newExceptT .
      (if outputYaml
      then writeByteStringOutput mOutFile . Yaml.encodePretty (Yaml.setConfCompare compare Yaml.defConfig)
      else writeLazyByteStringOutput mOutFile . encodePretty' (defConfig {confCompare = compare})) .
      unVotingProcedures $
      voteProcedures
