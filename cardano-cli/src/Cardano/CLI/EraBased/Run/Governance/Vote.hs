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

  shelleyBasedEraConstraints cOnwards $ do
    case voteStakeCred of
      AnyDRepVerificationKeyOrHashOrFile stake -> do
        DRepKeyHash h <- firstExceptT  GovernanceVoteCmdReadError
                                . newExceptT $ readVerificationKeyOrHashOrTextEnvFile AsDRepKey stake
        let vStakeCred = StakeCredentialByKey . StakeKeyHash $ coerceKeyRole h

        votingCred <- hoistEither $ first GovernanceVoteCmdCredentialDecodeError $ toVotingCredential cOnwards vStakeCred
        let voter = Ledger.DRepVoter (unVotingCredential votingCred)
            govActIdentifier = createGovernanceActionId govActionTxId govActionIndex
            voteProcedure = createVotingProcedure cOnwards voteChoice Nothing
            votingProcedures = singletonVotingProcedures cOnwards voter govActIdentifier (unVotingProcedure voteProcedure)
        firstExceptT GovernanceVoteCmdWriteError . newExceptT $ writeFileTextEnvelope oFp Nothing votingProcedures

      AnyStakePoolVerificationKeyOrHashOrFile stake -> do
        h <- firstExceptT GovernanceVoteCmdReadError
              . newExceptT $ readVerificationKeyOrHashOrTextEnvFile AsStakePoolKey stake

        let voter = Ledger.StakePoolVoter (unStakePoolKeyHash h)
            govActIdentifier = createGovernanceActionId govActionTxId govActionIndex
            voteProcedure = createVotingProcedure cOnwards voteChoice Nothing
            votingProcedures = singletonVotingProcedures cOnwards voter govActIdentifier (unVotingProcedure voteProcedure)
        firstExceptT GovernanceVoteCmdWriteError . newExceptT $ writeFileTextEnvelope oFp Nothing votingProcedures

      AnyCommitteeHotVerificationKeyOrHashOrFile stake -> do
        CommitteeHotKeyHash h <- firstExceptT GovernanceVoteCmdReadError
                                  . newExceptT $ readVerificationKeyOrHashOrTextEnvFile AsCommitteeHotKey stake
        let vStakeCred = StakeCredentialByKey . StakeKeyHash $ coerceKeyRole h
        votingCred <- hoistEither $ first GovernanceVoteCmdCredentialDecodeError $ toVotingCredential cOnwards vStakeCred
        let voter = Ledger.CommitteeVoter (Ledger.coerceKeyRole (unVotingCredential votingCred)) -- TODO Conway - remove coerceKeyRole
            govActIdentifier = createGovernanceActionId govActionTxId govActionIndex
            voteProcedure = createVotingProcedure cOnwards voteChoice Nothing
            votingProcedures = singletonVotingProcedures cOnwards voter govActIdentifier (unVotingProcedure voteProcedure)
        firstExceptT GovernanceVoteCmdWriteError . newExceptT $ writeFileTextEnvelope oFp Nothing votingProcedures
