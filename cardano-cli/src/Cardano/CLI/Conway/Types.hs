{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.CLI.Conway.Types where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Shelley.Key

import           Data.Text (Text)


type ConwayVoteFile = File ConwayVote

data ConwayVote
  = ConwayVote
    { cvVoteChoice :: VoteChoice
    , cvVoterType :: VType
    , cvGovActionTxIn :: TxIn
    , cvVotingStakeCredential :: VerificationKeyOrFile StakePoolKey
    , cvEra :: AnyShelleyBasedEra
    , cvFilepath :: ConwayVoteFile Out
    } deriving Show

type NewConstitutionFile = File NewConstitution

data NewConstitution
  = NewConstitution
      { ncEra :: AnyShelleyBasedEra
      , ncDeposit :: Lovelace
      , ncVotingStakeCredential :: VerificationKeyOrFile StakePoolKey
      , ncConstitution :: Constitution
      , ncFilePath :: NewConstitutionFile Out
      } deriving Show

data Constitution
  = ConstitutionFromFile (File () In)
  | ConstitutionFromText Text deriving Show

-- Vote type -- TODO: Conway era - remove me
data VType = VCC -- committee
           | VDR -- drep
           | VSP -- spo
           deriving Show


data AnyAtMostBabbageEra where
  AnyAtMostBabbageEra :: IsShelleyBasedEra era => AtMostBabbageEra era -> AnyAtMostBabbageEra

deriving instance Show AnyAtMostBabbageEra
