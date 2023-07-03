{-# LANGUAGE DataKinds #-}

module Cardano.CLI.Conway.Types where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Shelley.Key

import           Data.Text (Text)


type ConwayVoteFile = File ConwayVote

data ConwayVote
  = ConwayVote
    { cvVoteChoice :: VoteChoice
    , cvVoterType :: VoterType
    , cvGovActionTxIn :: TxIn
    , cvVotingStakeCredential :: (VerificationKeyOrFile StakePoolKey)
    , cvEra :: AnyShelleyBasedEra
    , cvFilepath :: ConwayVoteFile Out
    } deriving Show

type NewConstitutionFile = File NewConstitution

data NewConstitution
  = NewConstitution
      { ncEra                   :: AnyShelleyBasedEra
      , ncDeposit               :: Lovelace
      , ncVotingStakeCredential :: StakeIdentifier
      , ncConstitution :: Constitution
      , ncFilePath :: NewConstitutionFile Out
      } deriving Show

data Constitution
  = ConstitutionFromFile (File () In)
  | ConstitutionFromText Text deriving Show
