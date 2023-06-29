{-# LANGUAGE DataKinds #-}

module Cardano.CLI.Conway.Types where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Shelley.Key

data ConwayVote
  = ConwayVote
    { cvVoteChoice :: VoteChoice
    , cvVoterType :: VoterType
    , cvGovActionTxIn :: TxIn
    , cvVotingStakeCredential :: StakeIdentifier
    , cvEra :: AnyShelleyBasedEra
    , cvFilepath :: File () Out
    } deriving Show

data ConwayProposal
  = ConwayProposal
      { cpEra :: AnyShelleyBasedEra
      , cpDeposit :: Lovelace
      , cpVotingStakeCredential :: StakeIdentifier
      , cpFilePath :: File () Out
      } deriving Show
