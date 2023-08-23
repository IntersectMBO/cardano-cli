{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Run.Governance.Vote
  ( runGovernanceVoteCmds
  ) where

import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Commands.Governance.Vote
import           Cardano.CLI.Types.Errors.CmdError
import           Cardano.CLI.Types.Errors.GovernanceVoteCmdError
import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Key
import           Cardano.Ledger.Keys (coerceKeyRole)

import           Control.Monad.Except
import           Control.Monad.Trans.Except.Extra
import           Data.Bifunctor
import           Data.Function

runGovernanceVoteCmds :: ()
  => GovernanceVoteCmds era
  -> ExceptT CmdError IO ()
runGovernanceVoteCmds = \case
  GovernanceVoteCreateCmd anyVote ->
    runGovernanceVoteCreateCmd anyVote
      & firstExceptT CmdGovernanceVoteError

runGovernanceVoteCreateCmd
  :: AnyVote
  -> ExceptT GovernanceVoteCmdError IO ()
runGovernanceVoteCreateCmd (ConwayOnwardsVote cOnwards voteChoice (govActionTxId, govActionIndex) voteStakeCred oFp) = do
  let sbe = conwayEraOnwardsToShelleyBasedEra cOnwards -- TODO: Conway era - update vote creation related function to take ConwayEraOnwards
  case voteStakeCred of
    AnyDRepVerificationKeyOrHashOrFile stake -> do
      DRepKeyHash h <- firstExceptT  GovernanceVoteCmdReadError
                               . newExceptT $ readVerificationKeyOrHashOrTextEnvFile AsDRepKey stake
      let vStakeCred = StakeCredentialByKey . StakeKeyHash $ coerceKeyRole h

      votingCred <- hoistEither $ first GovernanceVoteCmdCredentialDecodeError $ toVotingCredential sbe vStakeCred
      let voter = VoterDRep votingCred
          govActIdentifier = shelleyBasedEraConstraints sbe $ createGovernanceActionId govActionTxId govActionIndex
          voteProcedure = createVotingProcedure sbe voteChoice Nothing
          votingEntry = VotingEntry { votingEntryVoter = voter
                                    , votingEntryGovActionId = GovernanceActionId govActIdentifier
                                    , votingEntryVotingProcedure = voteProcedure
                                    }
      firstExceptT GovernanceVoteCmdWriteError . newExceptT $ shelleyBasedEraConstraints sbe
        $ writeFileTextEnvelope oFp Nothing votingEntry

    AnyStakePoolVerificationKeyOrHashOrFile stake -> do
      h <- firstExceptT GovernanceVoteCmdReadError
             . newExceptT $ readVerificationKeyOrHashOrTextEnvFile AsStakePoolKey stake

      let voter = VoterSpo h
          govActIdentifier = shelleyBasedEraConstraints sbe $ createGovernanceActionId govActionTxId govActionIndex
          voteProcedure = createVotingProcedure sbe voteChoice Nothing
          votingEntry = VotingEntry { votingEntryVoter = voter
                                    , votingEntryGovActionId = GovernanceActionId govActIdentifier
                                    , votingEntryVotingProcedure = voteProcedure
                                    }
      firstExceptT GovernanceVoteCmdWriteError . newExceptT $ shelleyBasedEraConstraints sbe
        $ writeFileTextEnvelope oFp Nothing votingEntry

    AnyCommitteeHotVerificationKeyOrHashOrFile stake -> do
      CommitteeHotKeyHash h <- firstExceptT GovernanceVoteCmdReadError
                                 . newExceptT $ readVerificationKeyOrHashOrTextEnvFile AsCommitteeHotKey stake
      let vStakeCred = StakeCredentialByKey . StakeKeyHash $ coerceKeyRole h
      votingCred <- hoistEither $ first GovernanceVoteCmdCredentialDecodeError $ toVotingCredential sbe vStakeCred
      let voter = VoterCommittee votingCred
          govActIdentifier = shelleyBasedEraConstraints sbe $ createGovernanceActionId govActionTxId govActionIndex
          voteProcedure = createVotingProcedure sbe voteChoice Nothing
          votingEntry = VotingEntry { votingEntryVoter = voter
                                    , votingEntryGovActionId = GovernanceActionId govActIdentifier
                                    , votingEntryVotingProcedure = voteProcedure
                                    }
      firstExceptT GovernanceVoteCmdWriteError . newExceptT $ shelleyBasedEraConstraints sbe
        $ writeFileTextEnvelope oFp Nothing votingEntry
