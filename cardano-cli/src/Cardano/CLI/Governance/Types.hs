{-# LANGUAGE DataKinds #-}

module Cardano.CLI.Governance.Types
  ( ConstitutionSource(..)
  , GovVote (..)
  , GovVoterType(..)
  , NewConstitution(..)
  , NewConstitutionFile
  , VoteFile
  ) where

import qualified Cardano.Api as Api
import           Cardano.Api.IO
import           Cardano.Api.Shelley (AnyShelleyBasedEra (..), Lovelace (..))
import qualified Cardano.Api.Shelley as Api

import           Cardano.CLI.Shelley.Key

import           Data.Text (Text)

type VoteFile = File GovVote

data GovVote = GovVote
  { cvVoteChoice :: Api.VoteChoice
  , cvVoterType :: GovVoterType
  , cvGovActionTxIn :: Api.TxIn
  , cvVotingStakeCredential :: VerificationKeyOrFile Api.StakePoolKey
  , cvEra :: Api.AnyShelleyBasedEra
  , cvFilepath :: VoteFile Out
  } deriving (Eq, Show)

type NewConstitutionFile = File NewConstitution

data NewConstitution = NewConstitution
  { ncEra :: AnyShelleyBasedEra
  , ncDeposit :: Lovelace
  , ncVotingStakeCredential :: VerificationKeyOrFile Api.StakePoolKey
  , ncConstitutionSource :: ConstitutionSource
  , ncFilePath :: NewConstitutionFile Out
  } deriving (Eq, Show)

data ConstitutionSource
  = ConstitutionSourceFile (File () In)
  | ConstitutionSourceText Text
  deriving (Eq, Show)

-- Voter type, TODO remove me
data GovVoterType
  = GovVoterTypeCommittee
  | GovVoterTypeDRep
  | GovVoterTypeSpo
  deriving (Eq, Show)
