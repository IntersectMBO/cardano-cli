{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.Types.Governance where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Key (DRepHashSource, VerificationKeyOrFile,
                   VerificationKeyOrHashOrFile)

import           Data.Word

type VoteFile = File ConwayVote

data ConwayVote
  = ConwayVote
    { cvVoteChoice :: Vote
    , cvVoterType :: VType
    , cvGovActionId :: (TxId, Word32)
    , cvVotingStakeCredential :: VerificationKeyOrFile StakePoolKey
    , cvEra :: AnyShelleyBasedEra
    , cvFilepath :: VoteFile Out
    } deriving Show

-- Vote type -- TODO: Conway era - remove me
data VType = VCC -- committee
           | VDR -- drep
           | VSP -- spo
           deriving Show

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

data VoteDelegationTarget
  = VoteDelegationTargetOfDRep DRepHashSource
  | VoteDelegationTargetOfAbstain
  | VoteDelegationTargetOfNoConfidence
  deriving (Eq, Show)
