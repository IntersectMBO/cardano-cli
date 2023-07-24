{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.CLI.Types.Governance where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Key

import           Data.Text (Text)

data ConwayVote era
  = ConwayVote
    { cvConwayOnwards :: ShelleyBasedEra era
    , cvVoteChoice :: Vote
    , cvVoterType :: VType
    , cvGovActionTxIn :: TxIn
    , cvVotingStakeCredential :: VerificationKeyOrFile StakePoolKey
    , cvFilepath :: File (ConwayVote era) Out
    } deriving Show

data NewConstitution era
  = NewConstitution
      { ncSbe :: ShelleyBasedEra era -- We shouldn't have to store both of these
      , ncW :: ConwayEraOnwards era -- We shouldn't have to store both of these
      , ncDeposit :: Lovelace
      , ncVotingStakeCredential :: VerificationKeyOrFile StakePoolKey
      , ncConstitution :: Constitution
      , ncFilePath :: File (NewConstitution era) Out
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
  AnyAtMostBabbageEra :: IsShelleyBasedEra era => ShelleyToBabbageEra era -> AnyAtMostBabbageEra

deriving instance Show AnyAtMostBabbageEra
