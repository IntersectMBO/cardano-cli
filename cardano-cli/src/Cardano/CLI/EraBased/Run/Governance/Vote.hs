{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Run.Governance.Vote
  ( runGovernanceVoteCmds
  , runGovernanceVoteCreateCmd
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Commands.Governance
import           Cardano.CLI.EraBased.Commands.Governance.Vote
import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Key

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT)
import           Data.Bifunctor

runGovernanceVoteCmds :: ()
  => GovernanceVoteCmds
  -> ExceptT GovernanceCmdError IO ()
runGovernanceVoteCmds = \case
  GovernanceVoteCreateCmd vChoice vType govActionTxIn votingStakeCred anyEra oFp ->
    runGovernanceVoteCreateCmd anyEra vChoice vType govActionTxIn votingStakeCred oFp

runGovernanceVoteCreateCmd
  :: AnyShelleyBasedEra
  -> Vote
  -> VType
  -> TxIn
  -> VerificationKeyOrFile StakePoolKey
  -> File () Out -- TODO Use File vote type
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
      firstExceptT WriteFileError . newExceptT
        $ shelleyBasedEraConstraints sbe
        $ writeFileTextEnvelope oFp Nothing voteProcedure

    VDR -> do
      votingCred <- hoistEither $ first VotingCredentialDecodeGovCmdEror $ toVotingCredential sbe vStakeCred
      let govActIdentifier = makeGoveranceActionId sbe govActionTxIn
          voteProcedure = createVotingProcedure sbe vChoice (VoterDRep votingCred) govActIdentifier
      firstExceptT WriteFileError . newExceptT
        $ shelleyBasedEraConstraints sbe
        $ writeFileTextEnvelope oFp Nothing voteProcedure

    VSP -> do
      let govActIdentifier = makeGoveranceActionId sbe govActionTxIn
          voteProcedure = createVotingProcedure sbe vChoice (VoterSpo stakePoolKeyHash) govActIdentifier
      firstExceptT WriteFileError . newExceptT
        $ shelleyBasedEraConstraints sbe
        $ writeFileTextEnvelope oFp Nothing voteProcedure
