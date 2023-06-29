{-# LANGUAGE DataKinds #-}

module Cardano.CLI.Conway.Types where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Shelley.Key

import           Data.Text (Text)

data ConwayVote
  = ConwayVote
    { cvVoteChoice :: VoteChoice
    , cvVoterType :: VoterType
    , cvGovActionTxIn :: TxIn
    , cvVotingStakeCredential :: StakeIdentifier
    , cvEra :: AnyShelleyBasedEra
    , cvFilepath :: File () Out
    } deriving Show

data NewConstitution
  = NewConstitution
      { ncEra :: AnyShelleyBasedEra
      , ncDeposit :: Lovelace
      , ncVotingStakeCredential :: StakeIdentifier
      , ncConstitution :: Constitution
      , ncFilePath :: File () Out
      } deriving Show

data Constitution
  = ConstitutionFromFile (File () In)
  | ConstitutionFromText Text deriving Show
