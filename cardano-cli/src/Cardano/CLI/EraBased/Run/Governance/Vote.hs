{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Run.Governance.Vote
  ( runGovernanceVoteCreateCmd
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Commands.Governance
import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Key

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT)
import           Data.Bifunctor

runGovernanceVoteCreateCmd
  :: AnyShelleyBasedEra
  -> Vote
  -> VType
  -> TxIn
  -> VerificationKeyOrFile StakePoolKey
  -> VoteFile Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceVoteCreateCmd anyEra vChoice vType govActionTxIn votingStakeCred oFp = do
  AnyShelleyBasedEra sbe <- pure anyEra
  vStakePoolKey <- firstExceptT ReadFileError . newExceptT $ readVerificationKeyOrFile AsStakePoolKey votingStakeCred
  let stakePoolKeyHash = verificationKeyHash vStakePoolKey
      vStakeCred = StakeCredentialByKey . (verificationKeyHash . castVerificationKey) $ vStakePoolKey
  case vType of
    VCC -> do
      votingCred <- hoistEither $ first VotingCredentialDecodeGovCmdEror $ toVotingCredential sbe vStakeCred
      let govActIdentifier = makeGoveranceActionId sbe govActionTxIn
          voteProcedure = createVotingProcedure sbe vChoice (VoterCommittee votingCred) govActIdentifier
      firstExceptT WriteFileError . newExceptT $ obtainEraPParamsConstraint sbe $ writeFileTextEnvelope oFp Nothing voteProcedure

    VDR -> do
      votingCred <- hoistEither $ first VotingCredentialDecodeGovCmdEror $ toVotingCredential sbe vStakeCred
      let govActIdentifier = makeGoveranceActionId sbe govActionTxIn
          voteProcedure = createVotingProcedure sbe vChoice (VoterDRep votingCred) govActIdentifier
      firstExceptT WriteFileError . newExceptT $ obtainEraPParamsConstraint sbe $ writeFileTextEnvelope oFp Nothing voteProcedure

    VSP -> do
      let govActIdentifier = makeGoveranceActionId sbe govActionTxIn
          voteProcedure = createVotingProcedure sbe vChoice (VoterSpo stakePoolKeyHash) govActIdentifier
      firstExceptT WriteFileError . newExceptT $ obtainEraPParamsConstraint sbe $ writeFileTextEnvelope oFp Nothing voteProcedure
