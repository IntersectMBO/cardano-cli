{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Run.Governance.Vote
  ( runGovernanceVoteCmds
  ) where

import           Cardano.Api
import           Cardano.Api.Ledger (HasKeyRole (coerceKeyRole))
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Commands.Governance.Vote
import           Cardano.CLI.Types.Errors.CmdError
import           Cardano.CLI.Types.Errors.GovernanceVoteCmdError
import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Key

import           Control.Monad.Except
import           Control.Monad.Trans.Except.Extra
import           Data.Bifunctor
import           Data.Function

runGovernanceVoteCmds :: ()
  => GovernanceVoteCmds era
  -> ExceptT CmdError IO ()
runGovernanceVoteCmds = \case
  GovernanceVoteCreateCmd anyVote outFp ->
    runGovernanceVoteCreateCmd anyVote outFp
      & firstExceptT CmdGovernanceVoteError

runGovernanceVoteCreateCmd
  :: AnyVote
  -> File () Out
  -> ExceptT GovernanceVoteCmdError IO ()
runGovernanceVoteCreateCmd (ConwayOnwardsVote cOnwards v govTxInIdentifier anyStake) outFp = do
  case anyStake of
    AnyDRepVerificationKeyOrHashOrFile stake -> do
      let sbe = conwayEraOnwardsToShelleyBasedEra cOnwards -- TODO: Conway era - update vote creation related function to take ConwayEraOnwards
      DRepKeyHash h <- firstExceptT  GovernanceVoteCmdReadError
                         . newExceptT $ readVerificationKeyOrHashOrTextEnvFile AsDRepKey stake
      let vStakeCred = StakeCredentialByKey . StakeKeyHash $ coerceKeyRole h

      votingCred <- hoistEither $ first GovernanceVoteCmdCredentialDecodeError $ toVotingCredential sbe vStakeCred
      let govActIdentifier = makeGoveranceActionId sbe govTxInIdentifier
          voteProcedure = createVotingProcedure sbe v (VoterDRep votingCred) govActIdentifier
      firstExceptT GovernanceVoteCmeWriteError . newExceptT
        $ shelleyBasedEraConstraints sbe $ writeFileTextEnvelope outFp Nothing voteProcedure

    AnyStakePoolVerificationKeyOrHashOrFile stake -> do
      let sbe = conwayEraOnwardsToShelleyBasedEra cOnwards -- TODO: Conway era - update vote creation related function to take ConwayEraOnwards
      h <- firstExceptT GovernanceVoteCmdReadError
             . newExceptT $ readVerificationKeyOrHashOrTextEnvFile AsStakePoolKey stake

      let govActIdentifier = makeGoveranceActionId sbe govTxInIdentifier
          voteProcedure = createVotingProcedure sbe v (VoterSpo h) govActIdentifier
      firstExceptT GovernanceVoteCmeWriteError . newExceptT
        $ shelleyBasedEraConstraints sbe $ writeFileTextEnvelope outFp Nothing voteProcedure

    AnyCommitteeHotVerificationKeyOrHashOrFile stake -> do
      let sbe = conwayEraOnwardsToShelleyBasedEra cOnwards -- TODO: Conway era - update vote creation related function to take ConwayEraOnwards
      CommitteeHotKeyHash h <- firstExceptT GovernanceVoteCmdReadError
             . newExceptT $ readVerificationKeyOrHashOrTextEnvFile AsCommitteeHotKey stake
      let vStakeCred = StakeCredentialByKey . StakeKeyHash $ coerceKeyRole h
      votingCred <- hoistEither $ first GovernanceVoteCmdCredentialDecodeError $ toVotingCredential sbe vStakeCred

      let govActIdentifier = makeGoveranceActionId sbe govTxInIdentifier
          voteProcedure = createVotingProcedure sbe v (VoterCommittee votingCred) govActIdentifier
      firstExceptT GovernanceVoteCmeWriteError . newExceptT
        $ shelleyBasedEraConstraints sbe $ writeFileTextEnvelope outFp Nothing voteProcedure
