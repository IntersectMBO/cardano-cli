{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.Types.Governance where

import           Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key (VerificationKeyOrFile, VerificationKeyOrHashOrFile)

import           Data.Text (Text)
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

type NewConstitutionFile = File NewConstitution

data NewConstitution
  = NewConstitution
      { ncNetwork :: Ledger.Network
      , ncEra :: AnyShelleyBasedEra
      , ncDeposit :: Lovelace
      , ncVotingStakeCredential :: VerificationKeyOrFile StakePoolKey
      , ncPrevGovActId :: Maybe (TxId, Word32)
      , ncPropAnchor :: (Ledger.Url, Text)
      , ncConstitution :: Constitution
      , ncFilePath :: NewConstitutionFile Out
      } deriving Show

-- Vote type -- TODO: Conway era - remove me
data VType = VCC -- committee
           | VDR -- drep
           | VSP -- spo
           deriving Show

data AnyVote where
  ConwayOnwardsVote
    :: ConwayEraOnwards era
    -> Vote
    -> (TxId, Word32)
    -> AnyVotingStakeVerificationKeyOrHashOrFile
    -> VoteFile Out
    -> AnyVote

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
