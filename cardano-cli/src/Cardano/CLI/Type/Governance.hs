{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.Type.Governance where

import Cardano.Api

import Cardano.CLI.Type.Key
  ( DRepHashSource
  , StakePoolKeyHashSource
  , VerificationKeyOrHashOrFileOrScriptHash
  )

data ConwayVote

type VoteFile = File ConwayVote

-- Vote type -- TODO: Conway era - remove me
data VType
  = VCC -- committee
  | VDR -- drep
  | VSP -- spo
  deriving Show

-- | Possible credentials for creating a vote
data AnyVotingStakeVerificationKeyOrHashOrFile
  = AnyDRepVerificationKeyOrHashOrFileOrScriptHash (VerificationKeyOrHashOrFileOrScriptHash DRepKey)
  | AnyStakePoolVerificationKeyOrHashOrFile StakePoolKeyHashSource
  | AnyCommitteeHotVerificationKeyOrHashOrFileOrScriptHash
      (VerificationKeyOrHashOrFileOrScriptHash CommitteeHotKey)

data VoteDelegationTarget
  = VoteDelegationTargetOfDRep DRepHashSource
  | VoteDelegationTargetOfAbstain
  | VoteDelegationTargetOfNoConfidence
  deriving (Eq, Show)
