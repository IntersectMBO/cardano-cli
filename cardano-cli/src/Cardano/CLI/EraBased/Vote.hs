{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Vote where

import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Key

data AnyVote where
  ConwayOnwardsVote
    :: ConwayEraOnwards era
    -> Vote
    -> VType
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
