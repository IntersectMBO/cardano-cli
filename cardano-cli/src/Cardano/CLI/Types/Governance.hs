{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.Types.Governance where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Key (DRepHashSource, VerificationKeyOrFile,
                   VerificationKeyOrHashOrFile, VerificationKeyOrHashOrFileOrScriptHash)

import           Data.Word

type VoteFile = File ConwayVote

data ConwayVote
  = ConwayVote
    { cvVoteChoice :: Vote
    , cvVoterType :: VType
    , cvGovActionId :: (TxId, Word16)
    , cvVotingStakeCredential :: VerificationKeyOrFile StakePoolKey
    , cvEra :: AnyShelleyBasedEra
    , cvFilepath :: VoteFile Out
    } deriving Show

-- Vote type -- TODO: Conway era - remove me
data VType = VCC -- committee
           | VDR -- drep
           | VSP -- spo
           deriving Show

-- | Possible credentials for creating a vote
data AnyVotingStakeVerificationKeyOrHashOrFile =
    AnyDRepVerificationKeyOrHashOrFileOrScriptHash (VerificationKeyOrHashOrFileOrScriptHash DRepKey)
  | AnyStakePoolVerificationKeyOrHashOrFile (VerificationKeyOrHashOrFile StakePoolKey)
  | AnyCommitteeHotVerificationKeyOrHashOrFileOrScriptHash (VerificationKeyOrHashOrFileOrScriptHash CommitteeHotKey)

data VoteDelegationTarget
  = VoteDelegationTargetOfDRep DRepHashSource
  | VoteDelegationTargetOfAbstain
  | VoteDelegationTargetOfNoConfidence
  deriving (Eq, Show)
