{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Vote where

import           Cardano.Api.Ledger (HasKeyRole (coerceKeyRole))
import           Cardano.Api.Shelley

import           Cardano.Binary (DecoderError)
import           Cardano.CLI.Types.Key

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Data.Bifunctor

data AnyVote where
  ConwayOnwardsVote
    :: ConwayEraOnwards era
    -> Vote
    -> TxIn
    -> AnyVotingStakeVerificationKeyOrHashOrFile
    -> AnyVote

data AnyVotingStakeVerificationKeyOrHashOrFile where
  AnyDRepVerificationKeyOrHashOrFile
    :: VerificationKeyOrHashOrFile DRepKey
    -> AnyVotingStakeVerificationKeyOrHashOrFile

  AnyStakePoolVerificationKeyOrHashOrFile
    :: VerificationKeyOrHashOrFile StakePoolKey
    -> AnyVotingStakeVerificationKeyOrHashOrFile

  AnyCommitteeHotVerificationKeyOrHashOrFile
    :: VerificationKeyOrHashOrFile CommitteeHotKey
    -> AnyVotingStakeVerificationKeyOrHashOrFile

data EraBasedVoteError
  = EraBasedVoteReadError !(FileError InputDecodeError)
  | EraBasedVotingCredentialDecodeError !DecoderError
  | EraBasedVoteWriteError !(FileError ())

runGovernanceVote
  :: AnyVote
  -> File () Out
  -> ExceptT EraBasedVoteError IO ()
runGovernanceVote (ConwayOnwardsVote cOnwards v govTxInIdentifier anyStake) outFp =
  case anyStake of
    AnyDRepVerificationKeyOrHashOrFile stake -> do
      let sbe = conwayEraOnwardsToShelleyBasedEra cOnwards -- TODO: Conway era - update vote creation related function to take ConwayEraOnwards
      DRepKeyHash h <- firstExceptT  EraBasedVoteReadError
                         . newExceptT $ readVerificationKeyOrHashOrTextEnvFile AsDRepKey stake
      let vStakeCred = StakeCredentialByKey . StakeKeyHash $ coerceKeyRole h

      votingCred <- hoistEither $ first EraBasedVotingCredentialDecodeError $ toVotingCredential sbe vStakeCred
      let govActIdentifier = makeGoveranceActionId sbe govTxInIdentifier
          voteProcedure = createVotingProcedure sbe v (VoterDRep votingCred) govActIdentifier
      firstExceptT EraBasedVoteWriteError . newExceptT
        $ shelleyBasedEraConstraints sbe $ writeFileTextEnvelope outFp Nothing voteProcedure

    AnyStakePoolVerificationKeyOrHashOrFile stake -> do
      let sbe = conwayEraOnwardsToShelleyBasedEra cOnwards -- TODO: Conway era - update vote creation related function to take ConwayEraOnwards
      h <- firstExceptT EraBasedVoteReadError
             . newExceptT $ readVerificationKeyOrHashOrTextEnvFile AsStakePoolKey stake

      let govActIdentifier = makeGoveranceActionId sbe govTxInIdentifier
          voteProcedure = createVotingProcedure sbe v (VoterSpo h) govActIdentifier
      firstExceptT EraBasedVoteWriteError . newExceptT
        $ shelleyBasedEraConstraints sbe $ writeFileTextEnvelope outFp Nothing voteProcedure

    AnyCommitteeHotVerificationKeyOrHashOrFile stake -> do
      let sbe = conwayEraOnwardsToShelleyBasedEra cOnwards -- TODO: Conway era - update vote creation related function to take ConwayEraOnwards
      CommitteeHotKeyHash h <- firstExceptT EraBasedVoteReadError
             . newExceptT $ readVerificationKeyOrHashOrTextEnvFile AsCommitteeHotKey stake
      let vStakeCred = StakeCredentialByKey . StakeKeyHash $ coerceKeyRole h
      votingCred <- hoistEither $ first EraBasedVotingCredentialDecodeError $ toVotingCredential sbe vStakeCred

      let govActIdentifier = makeGoveranceActionId sbe govTxInIdentifier
          voteProcedure = createVotingProcedure sbe v (VoterCommittee votingCred) govActIdentifier
      firstExceptT EraBasedVoteWriteError . newExceptT
        $ shelleyBasedEraConstraints sbe $ writeFileTextEnvelope outFp Nothing voteProcedure
