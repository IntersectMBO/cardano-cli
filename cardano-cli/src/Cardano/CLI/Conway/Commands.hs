{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.Conway.Commands where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.Binary (DecoderError)
import           Cardano.CLI.Shelley.Key
import           Cardano.CLI.Shelley.Run.StakeAddress

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT)
import           Data.Bifunctor


data GovernanceCmdError
  = -- Voting related
    StakeCredGovCmdError ShelleyStakeAddressCmdError
  | VotingCredentialDecodeGovCmdEror DecoderError
  | WriteFileError (FileError ())
    -- Governance action related

runGovernanceCreateVoteCmd
  :: AnyShelleyBasedEra
  -> VoteChoice
  -> VoterType
  -> TxIn
  -> StakeIdentifier
  -> File () Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceCreateVoteCmd (AnyShelleyBasedEra sbe) vChoice cType govActionTxIn votingStakeCred oFp = do
  stakeCred <- firstExceptT StakeCredGovCmdError $ getStakeCredentialFromIdentifier votingStakeCred
  votingCred <- hoistEither $ first VotingCredentialDecodeGovCmdEror $ toVotingCredential sbe stakeCred
  let govActIdentifier = makeGoveranceActionIdentifier sbe govActionTxIn
      voteProcedure = createVotingProcedure sbe vChoice cType govActIdentifier votingCred
  firstExceptT WriteFileError . newExceptT $ writeFileTextEnvelope oFp Nothing voteProcedure

runGovernanceCreateActionCmd
  :: AnyShelleyBasedEra
  -> Lovelace
  -> Hash StakeKey
  -> GovernanceAction
  -> File () Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceCreateActionCmd (AnyShelleyBasedEra sbe) deposit depositReturnAddr govAction oFp =
  let proposal = createProposalProcedure sbe deposit depositReturnAddr govAction
  in firstExceptT WriteFileError . newExceptT $ writeFileTextEnvelope oFp Nothing proposal

