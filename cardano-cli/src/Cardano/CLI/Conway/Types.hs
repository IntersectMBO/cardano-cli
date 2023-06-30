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
    } deriving Show
